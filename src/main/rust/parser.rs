
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
use nom::sequence::tuple;
use nom::IResult;
use std::collections;
use std::str::FromStr;

fn sp(input: &str) -> IResult<&str, &str, VerboseError<&str>> {
    take_while(|c: char| c.is_ascii_whitespace())(input)
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
        graph::graph::AttributeValue::from,
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
            many0(preceded(preceded(sp, tag(",")), key_value_pair)),
        ),
        |(head, tail)| std::iter::once(head).chain(tail).collect::<collections::HashMap<_, _>>(),
    )(input)
}

fn attr_list (input: &str) -> IResult<&str, collections::HashMap<String, graph::graph::AttributeValue>, VerboseError<&str>> {
    map(
        delimited(
            preceded(sp, tag("[")),
            opt(key_value_pairs),
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
    tuple ( (
        opt ( map (tag ("strict"), |_| String::from ("strict") ) ),
        alt ( (
            map (tag ("digraph"), graph_type_digraph),
            map (tag ("graph"), graph_type_graph)
        ) ),
        opt ( dot_id ),
        stmt_list
    ) ) (input)
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
        let expected_integer = (String::from ("key"), graph::graph::AttributeValue::from(42));

        let input_float = "key=1.1";
        let input_integer = "key=42";

        assert_eq! (key_value_pair (input_float).unwrap ().1, expected_float);
        assert_eq! (key_value_pair (input_integer).unwrap ().1, expected_integer);
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

        assert_eq! (parse (input_graph).unwrap (), expected_graph);
    }
}


