
use std::io::Write;

pub mod dot;
pub mod error;
pub mod parser;

pub fn from_str (dot: &str)
    -> Result<Vec<graph::graph::LabelledGraph>, error::GraphDotError>
{
    parser::parse (dot)
}

pub fn graph_to_dot (output: &mut dyn Write, graph: &graph::graph::LabelledGraph)
    -> Result<(), error::GraphDotError>
{
    write! (output, "digraph {} {{\n", graph.graph ().name ())?;
    write! (output, "\tgraph [overlap=false;splines=true;]\n")?;
    for vertex_id in graph.vertices ()
    {
        let vertex_label = graph.vertex_label (vertex_id)?;
        write! (output, "\t{} [label=\"{}\"];\n", vertex_id, vertex_label)?;
    }
    for ((a,b), _weight) in graph.edges ()
    {
        write! (output, "\t{} -> {};\n", a, b)?;
    }
    write! (output, "}}\n")?;
    Ok (())
}

