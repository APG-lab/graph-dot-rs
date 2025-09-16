
use std::io::Write;

pub mod dot;
pub mod error;
pub mod parser;

pub fn elide_empty_str (s: &str)
    -> String
{
    if s.is_empty ()
    {
        String::from ("")
    }
    else
    {
        format! ("\"{}\" ", s)
    }
}

pub fn from_str (dot: &str)
    -> Result<Vec<graph::graph::LabelledGraph>, error::GraphDotError>
{
    parser::parse (dot)
}

pub fn tree_from_str (dot: &str)
    -> Result<(graph::graph::LabelledGraph, Vec<graph::graph::LabelledGraph>), error::GraphDotError>
{
    parser::parse_tree (dot)
}

pub fn graph_to_dot (output: &mut dyn Write, graph: &graph::graph::Graph)
    -> Result<(), error::GraphDotError>
{
    write! (output, "digraph \"{}\" {{\n", graph.name ())?;
    for vertex_id in graph.vertices ()
    {
        write! (output, "\t{}\n", vertex_id)?;
    }
    for ((a,b), _weight) in graph.edges ()
    {
        write! (output, "\t{} -> {}\n", a, b)?;
    }
    write! (output, "}}\n")?;
    Ok (())
}

pub fn labelled_graph_body_to_dot (output: &mut dyn Write, graph: &graph::graph::LabelledGraph)
    -> Result<(), error::GraphDotError>
{
    for vertex_id in graph.vertices ()
    {
        let vertex_label = graph.vertex_label (vertex_id)?;
        let (_vertex_id, vertex_attr_map) = graph.vertex_attrs (&vertex_label)?;
        let attr_stmts = vertex_attr_map.iter ()
            .map (|(k, v)| {
                let vv = match v
                {
                    l @ graph::graph::AttributeValue::BooleanLiteral (_)
                        | l @ graph::graph::AttributeValue::FloatLiteral (_)
                        | l @ graph::graph::AttributeValue::IntegerLiteral (_) => format! ("{}", l),
                    rest => format! ("\"{}\"", rest)
                };
                format! ("\"{}\"={}", k, vv)
            })
            .collect::<Vec<_>> ();

        if attr_stmts.is_empty ()
        {
            write! (output, "\t\"{}\"\n", vertex_label)?;
        }
        else
        {
            write! (output, "\t\"{}\" [{}]\n", vertex_label, attr_stmts.join (";"))?;
        }
    }
    for ((a,b), _weight) in graph.edges ()
    {
        let al = graph.vertex_label (a)?;
        let bl = graph.vertex_label (b)?;
        write! (output, "\t\"{}\" -> \"{}\"\n", al, bl)?;
    }
    Ok (())
}

pub fn labelled_graph_to_dot (output: &mut dyn Write, graph: &graph::graph::LabelledGraph)
    -> Result<(), error::GraphDotError>
{
    write! (output, "digraph \"{}\" {{\n", graph.graph ().name ())?;
    labelled_graph_body_to_dot (output, graph)?;
    write! (output, "}}\n")?;
    Ok (())
}

pub fn labelled_graph_to_dot_gviz (output: &mut dyn Write, graph: &graph::graph::LabelledGraph)
    -> Result<(), error::GraphDotError>
{
    write! (output, "digraph {} {{\n", graph.graph ().name ())?;
    write! (output, "\tgraph [overlap=false;splines=true;]\n")?;
    for vertex_id in graph.vertices ()
    {
        let vertex_label = graph.vertex_label (vertex_id)?;
        write! (output, "\t{} [label=\"{}\"]\n", vertex_id, vertex_label)?;
    }
    for ((a,b), _weight) in graph.edges ()
    {
        write! (output, "\t{} -> {};\n", a, b)?;
    }
    write! (output, "}}\n")?;
    Ok (())
}

pub fn labelled_graph_tree_to_dot (output: &mut dyn Write, tree: &graph::graph::LabelledGraph, g_vec: &Vec<graph::graph::LabelledGraph>)
    -> Result<(), error::GraphDotError>
{
    let roots = tree.graph ().sources ();
    match roots.len ()
    {
        0 => Err (error::GraphDotError::GraphError (String::from ("No root found"))),
        1 => {
            let root = roots.into_iter ()
                .next ()
                .unwrap ();
            //debug! ("root: {}", root);
            let root_graph = g_vec.get (root)
                .ok_or (error::GraphDotError::GraphError (format! ("Failed to get root {} from g_vec", root)))?;

            let mut q = Vec::<(usize,usize)>::with_capacity (tree.graph ().vertices ().len ());

            write! (output, "digraph {}{{\n", elide_empty_str (&root_graph.graph ().name ()))?;
            q.push ( ( root, 0 ) );
            for (e, d) in graph::algo::dfs_edges (tree.graph (), root)?
            {
                while let Some ( ( qli, qld ) ) = q.last ().copied () && qli != e.0
                {
                    let graph = g_vec.get (qli)
                        .ok_or (error::GraphDotError::GraphError (format! ("Failed to get vertex {} from g_vec", qli)))?;
                    labelled_graph_body_to_dot (output, graph)?;
                    let qldm = qld - 1;
                    write! (output, "\t{:\t>qldm$}}}\n", "")?;
                    q.pop ();
                }
                let graph = g_vec.get (e.1)
                    .ok_or (error::GraphDotError::GraphError (format! ("Failed to get vertex {} from g_vec", e.1)))?;
                write! (output, "\t{:\t>d$}subgraph \"{}\" {{\n", "", graph.graph ().name ())?;
                q.push ( ( e.1, d + 1 ) );
            }

            while let Some ( ( qli, qld ) ) = q.pop ()
            {
                let graph = g_vec.get (qli)
                    .ok_or (error::GraphDotError::GraphError (format! ("Failed to get vertex {} from g_vec", qli)))?;
                labelled_graph_body_to_dot (output, graph)?;
                write! (output, "{:\t>qld$}}}\n", "")?;
            }

            Ok (())
        },
        n @ _ => Err (error::GraphDotError::GraphError (format! ("Multiple roots ({}) found", n)))
    }
}

#[cfg(test)]
mod tests_lib
{
    use crate::parser;
    //use log::debug;
    use std::collections;
    use std::sync;

    static INIT: sync::Once = sync::Once::new ();

    fn init ()
    {
        INIT.call_once (env_logger::init);
    }

    #[test]
    fn test_graph ()
    {
        init ();
        let mut g = graph::graph::Graph::new ();
        g.add_vertex_raw (1).expect ("Failed to add vertex");

        let mut buf = Vec::new();
        super::graph_to_dot (&mut buf, &g).expect ("Failed to serialise to dot");

        let s = String::from_utf8 (buf).expect ("Failed to convert dot bytes to string");
        let sg = super::from_str (s.as_str ()).expect ("Failed to parse dot");

        assert_eq! (sg.len (), 1, "should return a single graph");
        assert_eq! (sg[0].graph (), &g, "Should be equal:\n\n{:?}\n\nand:\n\n{:?}\n", sg[0].graph (), g);
    }

    #[test]
    fn test_labelled_graph ()
    {
        init ();
        let g = graph::graph::LabelledGraph::new_with_name ("foo@bar");

        let mut buf = Vec::new();
        super::labelled_graph_to_dot (&mut buf, &g).expect ("Failed to serialise to dot");

        let s = String::from_utf8 (buf).expect ("Failed to convert dot bytes to string");
        let sg = super::from_str (s.as_str ()).expect ("Failed to parse dot");

        assert_eq! (sg.len (), 1, "should return a single graph");
        assert! (graph::eq::labels_and_attrs_eq (&sg[0], &g).expect ("Failed eq check"), "Should be equal:\n\n{:?}\n\nand:\n\n{:?}\n", sg[0], g);
    }

    #[test]
    fn test_labelled_graph_edge ()
    {
        init ();
        let mut g = graph::graph::LabelledGraph::new_with_name ("foo@bar");
        g.add_edge (String::from ("lims.data.site/name"), String::from ("lims.data.site/id"), None).expect ("Failed to add edge lims.data.site/name -> lims.data.site/id");

        let mut buf = Vec::new();
        super::labelled_graph_to_dot (&mut buf, &g).expect ("Failed to serialise to dot");

        let s = String::from_utf8 (buf).expect ("Failed to convert dot bytes to string");
        let sg = super::from_str (s.as_str ()).expect ("Failed to parse dot");

        assert_eq! (sg.len (), 1, "should return a single graph");
        assert! (graph::eq::labels_and_attrs_eq (&sg[0], &g).expect ("Failed eq check"), "Should be equal:\n\n{:?}\n\nand:\n\n{:?}\n", sg[0], g);
    }

    #[test]
    fn test_labelled_graph_vertex ()
    {
        init ();
        let mut g = graph::graph::LabelledGraph::new_with_name ("foo@bar");
        g.add_vertex (String::from ("lims.data.site/id"), None).expect ("Failed to add vertex lims.data.site/id");

        let mut buf = Vec::new();
        super::labelled_graph_to_dot (&mut buf, &g).expect ("Failed to serialise to dot");

        let s = String::from_utf8 (buf).expect ("Failed to convert dot bytes to string");
        let sg = super::from_str (s.as_str ()).expect ("Failed to parse dot");

        assert_eq! (sg.len (), 1, "should return a single graph");
        assert! (graph::eq::labels_and_attrs_eq (&sg[0], &g).expect ("Failed eq check"), "Should be equal:\n\n{:?}\n\nand:\n\n{:?}\n", sg[0], g);
    }

    #[test]
    fn test_vertex_attrs ()
    {
        init ();
        let mut g = graph::graph::LabelledGraph::new ();
        g.add_vertex (String::from ("graph"), Some (collections::HashMap::<String, graph::graph::AttributeValue>::from ([
            (String::from ("bold"), graph::graph::AttributeValue::BooleanLiteral (false)),
            (String::from ("label"), graph::graph::AttributeValue::StringLiteral (String::from ("graph")))
        ]))).expect ("Failed to add vertex");
        let mut buf = Vec::new();
        super::labelled_graph_to_dot (&mut buf, &g).expect ("Failed to serialise to dot");

        let s = String::from_utf8 (buf).expect ("Failed to convert dot bytes to string");
        let sg = super::from_str (s.as_str ()).expect ("Failed to parse dot");

        assert_eq! (sg.len (), 1, "should return a single graph");
        assert! (graph::eq::labels_and_attrs_eq (&sg[0], &g).expect ("Failed eq check"), "Should be equal:\n\n{:?}\n\nand:\n\n{:?}\n", sg[0], g);
    }

    #[test]
    fn test_labelled_graph_tree ()
    {
        init ();
        let mut tree = graph::graph::LabelledGraph::new ();
        tree.add_edge_raw (0, String::from (""), 1, String::from ("worker"), None, 0).expect ("Failed to add tree edge '' -> 'worker'");

        let mut g_worker = graph::graph::LabelledGraph::new_with_name ("worker");
        g_worker.add_edge (String::from ("lims.person/name"), String::from ("lims.person/id"), None).expect ("Failed to add edge lims.person/name -> lims.person/id");

        let mut g = graph::graph::LabelledGraph::new ();
        g.add_edge (String::from ("lims.data.site/name"), String::from ("lims.data.site/id"), None).expect ("Failed to add edge lims.data.site/name -> lims.data.site/id");

        let g_vec = vec![g_worker, g];
        let mut buf = Vec::new();
        super::labelled_graph_tree_to_dot (&mut buf, &tree, &g_vec).expect ("Failed to serialise to dot");

        let s = String::from_utf8 (buf).expect ("Failed to convert dot bytes to string");
        let sg = super::from_str (s.as_str ()).expect ("Failed to parse dot");

        assert_eq! (sg.len (), 2, "should return two graphs");
        assert! (graph::eq::labels_and_attrs_eq (&sg[0], &g_vec[0]).expect ("Failed eq check"), "Should be equal:\n\n{:?}\n\nand:\n\n{:?}\n", sg[0], g_vec[0]);

    }

    #[test]
    fn test_labelled_graph_tree_roundtrip_single ()
    {
        init ();

        let input_graph = r#"digraph {
}
"#;

        let (tree, g_vec) = parser::parse_tree (input_graph).expect ("Failed to parse tree");
        let mut buf = Vec::new();
        super::labelled_graph_tree_to_dot (&mut buf, &tree, &g_vec).expect ("Failed to serialise to dot");
        let output_graph = String::from_utf8 (buf).expect ("Failed to convert buffer to string");

        assert_eq! (input_graph, output_graph, "Should roundtrip (single graph)");
    }

    #[test]
    fn test_labelled_graph_tree_roundtrip_complex ()
    {
        init ();

        let input_graph = r#"digraph "1" {
	subgraph "9" {
		subgraph "10" {
		}
	}
	subgraph "5" {
	}
}
"#;

        let (tree, g_vec) = parser::parse_tree (input_graph).expect ("Failed to parse tree");
        let mut buf = Vec::new();
        super::labelled_graph_tree_to_dot (&mut buf, &tree, &g_vec).expect ("Failed to serialise to dot");
        let output_graph = String::from_utf8 (buf).expect ("Failed to convert buffer to string");

        assert_eq! (input_graph, output_graph, "Should roundtrip (complex graph)");
    }
}

