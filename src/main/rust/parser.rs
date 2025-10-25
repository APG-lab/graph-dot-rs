
use crate::dot;
use crate::error;
use graph;
use nom::branch::alt;
use nom::bytes::complete::escaped;
use nom::bytes::complete::tag;
use nom::bytes::complete::take_while;
use nom::character::complete::alpha1;
use nom::character::complete::alphanumeric1;
use nom::character::complete::digit0;
use nom::character::complete::digit1;
use nom::character::complete::none_of;
use nom::character::complete::space1;
use nom::combinator::cut;
use nom::combinator::map;
use nom::combinator::opt;
use nom::combinator::recognize;
use nom::error::context;
use nom::error::convert_error;
use nom::error::VerboseError;
use nom::multi::many0;
use nom::multi::many1;
use nom::sequence::delimited;
use nom::sequence::pair;
use nom::sequence::preceded;
use nom::sequence::terminated;
use nom::sequence::tuple;
use nom::IResult;
use std::collections;
use std::str::FromStr;

fn sp(input: &str) -> IResult<&str, &str, VerboseError<&str>> {
    take_while(|c: char| c.is_ascii_whitespace())(input)
}

fn string_within_attribute_value (input: &str) -> IResult<&str, String, VerboseError<&str>> {
    map (
        many1 ( alt ((
            alphanumeric1,
            space1,
            tag ("."),
            tag ("_"),
            tag ("-"),
            tag ("/")
        ))),
        |sc| sc.into_iter ().collect::<String> ()
    )(input)
}

fn neg_sign(input: &str) -> IResult<&str, bool, VerboseError<&str>> {
    map(opt(tag("-")), |t| t.is_some())(input)
}

fn integer_literal(input: &str) -> IResult<&str, graph::graph::AttributeValue, VerboseError<&str>> {
    map(pair(neg_sign, digit1), |(is_negative, num)| {
        let mut num = i64::from_str(num).unwrap();
        if is_negative {
            num = -num;
        }
        graph::graph::AttributeValue::from (num)
    })(input)
}

fn float_literal(input: &str) -> IResult<&str, graph::graph::AttributeValue, VerboseError<&str>> {
    map(
        pair(neg_sign, recognize(tuple((digit0, tag("."), digit0)))),
        |(is_negative, num)| {
            let mut num = f64::from_str(num).unwrap();
            if is_negative {
                num = -num;
            }
            graph::graph::AttributeValue::from (num)
        },
    )(input)
}

fn key_value_pair_string (input: &str) -> IResult<&str, (String, String), VerboseError<&str>> {
    pair(
        string_within_attribute_value,
        map (preceded(tag(":"), cut(string_within_attribute_value)), String::from)
    )(input)
}

fn collection_string_array (input: &str) -> IResult<&str, graph::graph::AttributeValue, VerboseError<&str>> {
    map(
        pair(
            string_within_attribute_value,
            many0(preceded(tag(","), string_within_attribute_value))
        ),
        |(head, tail)| graph::graph::AttributeValue::from (std::iter::once(head).chain(tail).collect::<Vec<String>>()),
    )(input)
}

fn collection_string_map (input: &str) -> IResult<&str, graph::graph::AttributeValue, VerboseError<&str>> {
    map(
        pair(
            key_value_pair_string,
            many0(preceded(tag(","), key_value_pair_string)),
        ),
        |(head, tail)| graph::graph::AttributeValue::from (std::iter::once(head).chain(tail).collect::<collections::HashMap<String, String>>()),
    )(input)
}

fn collection_string_set (input: &str) -> IResult<&str, graph::graph::AttributeValue, VerboseError<&str>> {
    map(
        pair(
            string_within_attribute_value,
            many0(preceded(tag(","), string_within_attribute_value))
        ),
        |(head, tail)| graph::graph::AttributeValue::from (std::iter::once(head).chain(tail).collect::<collections::HashSet<String>>()),
    )(input)
}

fn parse_collection (input: &str)
    -> IResult<&str, graph::graph::AttributeValue, VerboseError<&str>>
{
    alt ( (
        delimited ( tag ("["), collection_string_array, tag ("]") ),
        delimited ( tag ("{"), collection_string_map, tag ("}") ),
        delimited ( tag ("{"), collection_string_set, tag ("}") )
            )) (input)
}

fn remove_first_and_last (value: &str)
    -> &str
{
    let mut chars = value.chars ();
    chars.next ();
    chars.next_back ();
    chars.as_str ()
}

fn string_to_attribute_value (input: &str)
    -> graph::graph::AttributeValue
{
    if ( input.starts_with ("[[") && input.ends_with ("]]") ) ||
        ( input.starts_with ("{{") && input.ends_with ("}}") )
    {
        graph::graph::AttributeValue::from (remove_first_and_last (input))
    }
    else
    {
        match parse_collection (input) {
            Ok (tt) => {
                tt.1
            },
            Err (nom::Err::Error (_)) | Err (nom::Err::Failure (_)) => {
                graph::graph::AttributeValue::from (input)
            },
            Err(nom::Err::Incomplete(_)) => {
                graph::graph::AttributeValue::from (input)
            }
        }
    }
}

fn single_quoted_string(input: &str) -> IResult<&str, &str, VerboseError<&str>> {
    let escaped = escaped(none_of("\\\'"), '\\', tag("'"));
    let escaped_or_empty = alt((escaped, tag("")));
    delimited(tag("'"), escaped_or_empty, tag("'"))(input)
}

fn double_quoted_string(input: &str) -> IResult<&str, &str, VerboseError<&str>> {
    let escaped = escaped(none_of("\\\""), '\\', tag("\""));
    let escaped_or_empty = alt((escaped, tag("")));
    delimited(tag("\""), escaped_or_empty, tag("\""))(input)
}

fn string_literal(input: &str) -> IResult<&str, graph::graph::AttributeValue, VerboseError<&str>> {
    map(
        alt((single_quoted_string, double_quoted_string)),
        string_to_attribute_value,
    )(input)
}

fn boolean_literal(input: &str) -> IResult<&str, graph::graph::AttributeValue, VerboseError<&str>> {
    alt((
        map(tag("true"), |_| graph::graph::AttributeValue::BooleanLiteral (true)),
        map(tag("TRUE"), |_| graph::graph::AttributeValue::BooleanLiteral (true)),
        map(tag("false"), |_| graph::graph::AttributeValue::BooleanLiteral (false)),
        map(tag("FALSE"), |_| graph::graph::AttributeValue::BooleanLiteral (false)),
    ))(input)
}

fn integer_string (input: &str) -> IResult<&str, &str, VerboseError<&str>> {
    context (
        "integer",
    alt ( (
            recognize ( pair(neg_sign, many1 (digit1) ) ),
            recognize ( many1 (digit1) )
    ) )
    ) (input)
}

fn string_string (input: &str) -> IResult<&str, &str, VerboseError<&str>> {
    alt((single_quoted_string, double_quoted_string)) (input)
}

fn dot_id (input: &str)
    -> IResult<&str, String, VerboseError<&str>>
{
    map (
        preceded (
            sp,
            alt ( (
                integer_string,
                string_string,
                recognize (
                    pair (
                        alt ( (
                            alpha1,
                            tag("_")
                        ) ),
                        many0(alt((alphanumeric1, tag("_"))))
                    )
                )
            ) )
        ),
        String::from
    ) (input)
}

fn literal (input: &str)
    -> IResult<&str, graph::graph::AttributeValue, VerboseError<&str>>
{
    preceded(
        sp,
        alt((
            float_literal,
            integer_literal,
            string_literal,
            boolean_literal,
        )),
    )(input)
}

fn key_value_pair(input: &str) -> IResult<&str, (String, graph::graph::AttributeValue), VerboseError<&str>> {
    pair(
        dot_id,
        preceded(preceded(sp, tag("=")), cut(literal)),
    )(input)
}

fn key_value_pairs (input: &str) -> IResult<&str, collections::HashMap<String, graph::graph::AttributeValue>, VerboseError<&str>> {
    map(
        pair(
            key_value_pair,
            many0(preceded(preceded(sp, alt ( (tag(","), tag (";")) )), key_value_pair)),
        ),
        |(head, tail)| std::iter::once(head).chain(tail).collect::<collections::HashMap<_, _>>(),
    )(input)
}

fn attr_list (input: &str) -> IResult<&str, collections::HashMap<String, graph::graph::AttributeValue>, VerboseError<&str>> {
    map(
        delimited(
            preceded(sp, tag("[")),
            opt(terminated (key_value_pairs, opt (preceded(sp, alt ( (tag(","), tag (";")) ))))),
            preceded(sp, tag("]")),
        ),
        |properties| properties.unwrap_or_default(),
    )(input)
}

fn attr_type_edge<T> (_: T) -> dot::AttributeType {  dot::AttributeType::Edge }
fn attr_type_graph<T> (_: T) -> dot::AttributeType {  dot::AttributeType::Graph }
fn attr_type_node<T> (_: T) -> dot::AttributeType {  dot::AttributeType::Node }

fn attr_stmt (input: &str) -> IResult<&str, (dot::AttributeType, collections::HashMap<String, graph::graph::AttributeValue>), VerboseError<&str>>
{
    pair ( alt ( (map (tag ("edge"), attr_type_edge), map (tag ("graph"), attr_type_graph), map (tag ("node"), attr_type_node)) ), attr_list ) (input)
}

fn nodeid_or_subgraph_id (input: &str) -> IResult<&str, String, VerboseError<&str>>
{
    dot_id (input)
}

fn edge_op_directed<T> (_: T) -> dot::EdgeType { dot::EdgeType::Directed }
fn edge_op_undirected<T> (_: T) -> dot::EdgeType { dot::EdgeType::UnDirected }

fn edge_op (input: &str)
    -> IResult<&str, dot::EdgeType, VerboseError<&str>>
{
    preceded (sp, alt ( ( map (tag ("->"), edge_op_directed), map (tag ("--"), edge_op_undirected) ) ) ) (input)
}

fn edge_rhs (input: &str)
    -> IResult<&str, Box<dot::EdgeRightHandSide>, VerboseError<&str>>
{
    map (tuple ( ( edge_op, nodeid_or_subgraph_id, opt (edge_rhs) ) ), |t| Box::new (dot::EdgeRightHandSide { edge_op: t.0, rhs: t.1, rest: t.2 }) ) (input)
}

fn edge_stmt (input: &str)
    -> IResult<&str, (String, Box<dot::EdgeRightHandSide>, Option<collections::HashMap<String, graph::graph::AttributeValue>>), VerboseError<&str>>
{
    tuple ( ( nodeid_or_subgraph_id, edge_rhs, opt (attr_list) ) ) (input)
}

fn node_stmt (input: &str)
    -> IResult<&str, (String, Option<collections::HashMap<String, graph::graph::AttributeValue>>), VerboseError<&str>>
{
    pair ( dot_id, opt (attr_list) ) (input)
}

fn subg_stmt (input: &str)
    -> IResult<&str, (&str, Option<String>, Vec<dot::Statement>), VerboseError<&str>>
{
    preceded (
        sp,
        alt ( (
            map ( stmt_list, |v| ("subgraph", None::<String>, v)),
            tuple ( (tag ("subgraph"), opt (dot_id), stmt_list) )
        ) )
    ) (input)
}

fn stmt (input: &str) -> IResult<&str, dot::Statement, VerboseError<&str>>
{
    alt ( (
            map (attr_stmt, |s| dot::Statement::Attribute (s) ),
            map (edge_stmt, |s| dot::Statement::Edge (s) ),
            map (subg_stmt, |s| dot::Statement::SubGraph ( (s.1, s.2) ) ),
            map (node_stmt, |s| dot::Statement::Node (s) ) // We test node last, as it will match most things
        ) ) (input)
}

fn stmt_list (input: &str) -> IResult<&str, Vec<dot::Statement>, VerboseError<&str>>
{
    delimited (
        preceded (sp, tag ("{")),
        many0 (stmt),
        preceded (sp, tag ("}")),
    ) (input)
}

fn graph_type_digraph<T> (_: T) -> dot::GraphType { dot::GraphType::DiGraph }
fn graph_type_graph<T> (_: T) -> dot::GraphType { dot::GraphType::Graph }

fn parse_graph (input: &str) -> IResult<&str, (Option<String>, dot::GraphType, Option<String>, Vec<dot::Statement>), VerboseError<&str>>
{
    preceded (sp,
        tuple ( (
            opt ( map (tag ("strict"), |_| String::from ("strict") ) ),
            preceded (sp,
                alt ( (
                    map (tag ("digraph"), graph_type_digraph),
                    map (tag ("graph"), graph_type_graph)
                ) )
            ),
            opt ( dot_id ),
            stmt_list
        ) )
    ) (input)
}

pub(crate) fn parse (input: &str)
    -> Result<Vec<graph::graph::LabelledGraph>, error::GraphDotError>
{
    match parse_graph (input) {
        Ok (tt) => {
            dot::as_labelled_graphs (tt.1)
        },
        Err (nom::Err::Error (e)) | Err (nom::Err::Failure (e)) => {
            Err (error::GraphDotError::ParseError (convert_error (input, e)))
        },
        Err(nom::Err::Incomplete(_)) => {
            Err (error::GraphDotError::ParseError (String::from ("Input incomplete")))
        }
    }
}

pub(crate) fn parse_tree (input: &str)
    -> Result<(graph::graph::LabelledGraph, Vec<graph::graph::LabelledGraph>), error::GraphDotError>
{
    match parse_graph (input) {
        Ok (tt) => {
            dot::as_labelled_graphs_tree (tt.1)
        },
        Err (nom::Err::Error (e)) | Err (nom::Err::Failure (e)) => {
            Err (error::GraphDotError::ParseError (convert_error (input, e)))
        },
        Err(nom::Err::Incomplete(_)) => {
            Err (error::GraphDotError::ParseError (String::from ("Input incomplete")))
        }
    }
}

#[cfg(test)]
mod tests
{
    use super::*;
    use std::sync;

    static INIT: sync::Once = sync::Once::new ();

    fn init ()
    {
        INIT.call_once (env_logger::init);
    }

    #[test]
    fn test_attr ()
    {
        let expected_float = (String::from ("key"), graph::graph::AttributeValue::from (1.1));
        let expected_integer = (String::from ("key"), graph::graph::AttributeValue::from (42));
        let expected_string = (String::from ("key"), graph::graph::AttributeValue::from ("bar"));

        let input_float = "key=1.1";
        let input_integer = "key=42";
        let input_string = r#"key="bar""#;
        let input_string_qname = r#""key"="bar""#;

        assert_eq! (key_value_pair (input_float).unwrap ().1, expected_float);
        assert_eq! (key_value_pair (input_integer).unwrap ().1, expected_integer);
        assert_eq! (key_value_pair (input_string).unwrap ().1, expected_string);
        assert_eq! (key_value_pair (input_string_qname).unwrap ().1, expected_string);
    }

    #[test]
    fn test_attr_collection_string_array ()
    {
        let expected_string_array_char = (String::from ("key"), graph::graph::AttributeValue::from (vec![
            String::from ("a")
        ]));
        let expected_string_array_multi = (String::from ("key"), graph::graph::AttributeValue::from (vec![
            String::from ("a"),
            String::from ("b"),
            String::from ("a")
        ]));
        let expected_string_array_escaped = (String::from ("key"), graph::graph::AttributeValue::from (String::from ("[]")));

        let input_string_array_char = r#"key="[a]""#;
        let input_string_array_multi = r#"key="[a,b,a]""#;
        let input_string_array_escaped = r#"key="[[]]""#;

        assert_eq! (key_value_pair (input_string_array_char).unwrap ().1, expected_string_array_char);
        assert_eq! (key_value_pair (input_string_array_multi).unwrap ().1, expected_string_array_multi);
        assert_eq! (key_value_pair (input_string_array_escaped).unwrap ().1, expected_string_array_escaped);
    }

    #[test]
    fn test_attr_collection_string_map ()
    {
        let exepected_string_map_char = (String::from ("key"), graph::graph::AttributeValue::from (collections::HashMap::<String,String>::from ([
            ( String::from ("a"), String::from ("1") )
        ])));
        let exepected_string_map_string = (String::from ("key"), graph::graph::AttributeValue::from (collections::HashMap::<String,String>::from ([
            ( String::from ("aa"), String::from ("11") )
        ])));
        let exepected_string_map_special = (String::from ("key"), graph::graph::AttributeValue::from (collections::HashMap::<String,String>::from ([
            ( String::from ("./_-"), String::from ("./_-") )
        ])));
        let exepected_string_map_multi = (String::from ("key"), graph::graph::AttributeValue::from (collections::HashMap::<String,String>::from ([
            ( String::from ("a"), String::from ("1") ),
            ( String::from ("b"), String::from ("2") )
        ])));
        let expected_string_map_escaped = (String::from ("key"), graph::graph::AttributeValue::from (String::from ("{}")));

        let input_string_map_char = r#"key="{a:1}""#;
        let input_string_map_string = r#"key="{aa:11}""#;
        let input_string_map_special = r#"key="{./_-:./_-}""#;
        let input_string_map_multi = r#"key="{a:1,b:2}""#;
        let input_string_map_escaped = r#"key="{{}}""#;

        assert_eq! (key_value_pair (input_string_map_char).unwrap ().1, exepected_string_map_char);
        assert_eq! (key_value_pair (input_string_map_string).unwrap ().1, exepected_string_map_string);
        assert_eq! (key_value_pair (input_string_map_special).unwrap ().1, exepected_string_map_special);
        assert_eq! (key_value_pair (input_string_map_multi).unwrap ().1, exepected_string_map_multi);
        assert_eq! (key_value_pair (input_string_map_escaped).unwrap ().1, expected_string_map_escaped);
    }

    #[test]
    fn test_attr_collection_string_set ()
    {
        let expected_string_set_char = (String::from ("key"), graph::graph::AttributeValue::from (collections::HashSet::<String>::from ([
            String::from ("a")
        ])));
        let expected_string_set_multi = (String::from ("key"), graph::graph::AttributeValue::from (collections::HashSet::<String>::from ([
            String::from ("a"),
            String::from ("b")
        ])));

        let input_string_set_char = r#"key="{a}""#;
        let input_string_set_multi = r#"key="{a,b,a}""#;

        assert_eq! (key_value_pair (input_string_set_char).unwrap ().1, expected_string_set_char);
        assert_eq! (key_value_pair (input_string_set_multi).unwrap ().1, expected_string_set_multi);
    }

    #[test]
    fn test_attr_list ()
    {
        let expected_attr_list = collections::HashMap::from ([
            (String::from ("foo"), graph::graph::AttributeValue::from ("bar")),
            (String::from ("bar"), graph::graph::AttributeValue::from (2)),
        ]);
        let input_attr_list = "[foo='bar',bar=2]";

        assert_eq! (attr_list (input_attr_list).unwrap ().1, expected_attr_list);
    }

    #[test]
    fn test_attr_stmt ()
    {
        let expected_attr_stmt = (dot::AttributeType::Node, collections::HashMap::from ([(String::from ("foo"), graph::graph::AttributeValue::from ("bar"))]));
        let input_attr_stmt = "node [foo='bar']";
        assert_eq! (attr_stmt (input_attr_stmt).unwrap ().1, expected_attr_stmt);
    }

    #[test]
    fn test_dot_id ()
    {
        let expected_alpha = "a";
        let expected_alpha_str = "aa";
        let expected_numeric = "1";

        let input_alpha = "a";
        let input_alpha_str = "aa";
        let input_numeric = "1";

        assert_eq! (dot_id (input_alpha).unwrap ().1, expected_alpha);
        assert_eq! (dot_id (input_alpha_str).unwrap ().1, expected_alpha_str);
        assert_eq! (dot_id (input_numeric).unwrap ().1, expected_numeric);
    }

    #[test]
    fn test_edge_stmt ()
    {
        let edge_rhs = dot::EdgeRightHandSide { edge_op: dot::EdgeType::Directed, rhs: String::from ("b"), rest: None };
        let expected_edge_stmt = (String::from ("a"), Box::new (edge_rhs), None);
        let input_edge_stmt = "a -> b";

        assert_eq! (edge_stmt (input_edge_stmt).unwrap ().1, expected_edge_stmt);
    }

    #[test]
    fn test_node_stmt ()
    {
        let expected_node_stmt = (String::from ("a"), None);
        let input_node_stmt = "a";

        assert_eq! (node_stmt (input_node_stmt).unwrap ().1, expected_node_stmt);
    }

    #[test]
    fn test_node_stmt_with_attr_list ()
    {
        let expected_node_stmt = (String::from ("a"), Some (collections::HashMap::from ([(String::from ("foo"), graph::graph::AttributeValue::from ("bar"))])));
        let input_node_stmt = "a [foo='bar']";

        assert_eq! (node_stmt (input_node_stmt).unwrap ().1, expected_node_stmt);

    }

    #[test]
    fn test_node_stmt_with_attr_list_multi ()
    {
        let expected_node_stmt = (String::from ("a"), Some (collections::HashMap::from ([
                                                                                        (String::from ("foo"), graph::graph::AttributeValue::from ("bar")),
                                                                                        (String::from ("cat"), graph::graph::AttributeValue::from ("mat"))
            ])));
        let input_node_stmt_comma = "a [foo='bar',cat='mat']";
        let input_node_stmt_semi = "a [foo='bar';cat='mat']";
        let input_node_stmt_semi_term = "a [foo='bar';cat='mat';]";

        assert_eq! (node_stmt (input_node_stmt_comma).unwrap ().1, expected_node_stmt, "Failed node_stmt_comma");
        assert_eq! (node_stmt (input_node_stmt_semi).unwrap ().1, expected_node_stmt, "Failed node_stmt_semi");
        assert_eq! (node_stmt (input_node_stmt_semi_term).unwrap ().1, expected_node_stmt, "Failed node_stmt_semi_term");
    }

    #[test]
    fn test_stmt_list ()
    {
        init ();
        let expected_stmt_list = [];
        let input_stmt_list = "{}";

        assert_eq! (stmt_list (input_stmt_list).unwrap ().1, expected_stmt_list);
    }

    #[test]
    fn test_stmt_list_edges ()
    {
        init ();
        let expected_stmt_list = [dot::Statement::Edge ((String::from ("a"), Box::new (dot::EdgeRightHandSide { edge_op: dot::EdgeType::Directed, rhs: String::from ("b"), rest: None }), None))];
        let input_stmt_list = "{\n\ta -> b\n}";

        assert_eq! (stmt_list (input_stmt_list).unwrap ().1, expected_stmt_list);
    }

    #[test]
    fn test_parse ()
    {
        init ();
        let mut expected_graph = graph::graph::LabelledGraph::new ();
        expected_graph.add_edge (String::from ("a"), String::from ("b"), None).unwrap ();
        let input_graph = "digraph {\n\ta -> b\n}";

        assert_eq! (parse (input_graph).unwrap (), vec![expected_graph]);
    }

    #[test]
    fn test_parse_strict ()
    {
        init ();
        let mut expected_graph = graph::graph::LabelledGraph::new ();
        expected_graph.add_edge (String::from ("a"), String::from ("b"), None).unwrap ();
        let input_graph = "strict graph {\n\ta -> b\n}";

        assert_eq! (parse (input_graph).unwrap (), vec![expected_graph]);
    }

    #[test]
    fn test_parse_subgraph ()
    {
        init ();
        let mut expected_graph_bi   = graph::graph::LabelledGraph::new_with_name ("bI");
        let mut expected_graph_aii  = graph::graph::LabelledGraph::new_with_name ("aII");
        let mut expected_graph_ai   = graph::graph::LabelledGraph::new_with_name ("aI");
        let mut expected_graph_base = graph::graph::LabelledGraph::new ();

        expected_graph_bi.add_edge   (String::from ("g"), String::from ("h"), None).unwrap ();
        expected_graph_aii.add_edge  (String::from ("e"), String::from ("f"), None).unwrap ();
        expected_graph_ai.add_edge   (String::from ("c"), String::from ("d"), None).unwrap ();
        expected_graph_base.add_edge (String::from ("a"), String::from ("b"), None).unwrap ();

        let input_graph = r#"
digraph {
    subgraph bI {
        g -> h
    }

    subgraph aI {
        subgraph aII {
            e -> f
        }
        c -> d
    }
    a -> b
}"#;

        let expected_g_vec = vec![expected_graph_base, expected_graph_ai, expected_graph_aii, expected_graph_bi];
        assert_eq! (parse (input_graph).unwrap (), expected_g_vec);
    }

    #[test]
    fn test_parse_tree_subgraph ()
    {
        init ();
        let mut expected_graph_bi   = graph::graph::LabelledGraph::new_with_name ("bI");
        let mut expected_graph_aii  = graph::graph::LabelledGraph::new_with_name ("aII");
        let mut expected_graph_ai   = graph::graph::LabelledGraph::new_with_name ("aI");
        let mut expected_graph_base = graph::graph::LabelledGraph::new ();

        expected_graph_bi.add_edge   (String::from ("g"), String::from ("h"), None).unwrap ();
        expected_graph_aii.add_edge  (String::from ("e"), String::from ("f"), None).unwrap ();
        expected_graph_ai.add_edge   (String::from ("c"), String::from ("d"), None).unwrap ();
        expected_graph_base.add_edge (String::from ("a"), String::from ("b"), None).unwrap ();

        let input_graph = r#"
digraph {
    subgraph bI {
        g -> h
    }

    subgraph aI {
        subgraph aII {
            e -> f
        }
        c -> d
    }
    a -> b
}"#;

        let mut expected_tree = graph::graph::LabelledGraph::new ();
        expected_tree.add_edge_raw (0, String::from (""),   1, String::from ("bI"),  None, 0).unwrap ();
        expected_tree.add_edge_raw (0, String::from (""),   2, String::from ("aI"),  None, 0).unwrap ();
        expected_tree.add_edge_raw (2, String::from ("aI"), 3, String::from ("aII"), None, 0).unwrap ();
        let expected_g_vec = vec![expected_graph_base, expected_graph_bi, expected_graph_ai, expected_graph_aii];

        assert_eq! (parse_tree (input_graph).unwrap (), (expected_tree, expected_g_vec));
    }

    #[test]
    fn test_parse_white_space ()
    {
        init ();
        let mut expected_graph = graph::graph::LabelledGraph::new ();
        expected_graph.add_edge (String::from ("a"), String::from ("b"), None).unwrap ();
        let input_graph = "\n\tdigraph {\n\ta -> b\n}\n\t\n";

        assert_eq! (parse (input_graph).unwrap (), vec![expected_graph]);
    }
}


