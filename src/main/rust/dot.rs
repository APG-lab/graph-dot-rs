
use crate::error;
use graph;
use log::debug;
use std::collections;

#[derive(Clone, Debug, PartialEq)]
pub enum AttributeType
{
    Edge,
    Graph,
    Node
}

#[derive(Clone, Debug, PartialEq)]
pub enum EdgeType
{
    Directed,
    UnDirected
}

#[derive(Clone, Debug, PartialEq)]
pub struct EdgeRightHandSide
{
    pub edge_op: EdgeType,
    pub rhs: String,
    pub rest: Option<Box<Self>>
}

#[derive(Debug, PartialEq)]
pub enum GraphType
{
    DiGraph,
    Graph
}

#[derive(Debug, PartialEq)]
pub enum IdOrSubGraphId
{
    NodeId (String),
    SubGraphId (String)
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement
{
    Attribute ((AttributeType, collections::HashMap<String, graph::graph::AttributeValue>)),
    Edge ((String, Box<EdgeRightHandSide>, Option<collections::HashMap<String, graph::graph::AttributeValue>>)),
    Node ((String, Option<collections::HashMap<String, graph::graph::AttributeValue>>)),
    SubGraph ((Option<String>, Vec<Self>))
}

pub fn as_labelled_graph_node_erhs (g: &mut graph::graph::LabelledGraph, node_id_a: String, erhs: &EdgeRightHandSide, edge_attrs: Option<collections::HashMap<String, graph::graph::AttributeValue>>)
    -> Result<(), error::GraphDotError>
{
    match erhs
    {
        EdgeRightHandSide {edge_op: EdgeType::Directed, rhs: node_id_b, rest: _} => {
            g.add_edge (node_id_a, node_id_b.clone (), edge_attrs)?;
            Ok (())
        }
        _ => {
            debug! ("Unknown ehrs: {:?}", erhs);
            Ok (())
        }
    }
}

pub fn as_labelled_graph_node (g: &mut graph::graph::LabelledGraph, node_id_a: String, node_attrs: Option<collections::HashMap<String, graph::graph::AttributeValue>>)
    -> Result<(), error::GraphDotError>
{
    g.add_vertex (node_id_a, node_attrs)?;
    Ok (())
}

pub fn as_labelled_graph_internal (g: &mut graph::graph::LabelledGraph, subgraphs: &mut Vec::<graph::graph::LabelledGraph>, mut stmts: collections::VecDeque<Statement>)
    -> Result<(), error::GraphDotError>
{
    while let Some (stmt) = stmts.pop_front ()
    {
        //debug! ("stmt: {:?}", stmt);
        match stmt
        {
            Statement::Attribute (ref _a) => {},
            Statement::Edge (ref e) => {
                //debug! ("e: {:?}", e);
                match e
                {
                    (node_or_subgraph_id_a, ref erhs, opt_edge_attrs) => {
                        //debug! ("node_or_subgraph_id_a: {}", node_or_subgraph_id_a);
                        as_labelled_graph_node_erhs (g, node_or_subgraph_id_a.clone (), &erhs, opt_edge_attrs.clone ())?;
                    }
                }
            },
            Statement::Node (ref n) => {
                as_labelled_graph_node (g, n.0.clone (), n.1.clone ())?;
            },
            Statement::SubGraph (ref s) => {
                //debug! ("s: {:?}", s);
                let mut sg = graph::graph::LabelledGraph::new_with_name (&s.0.clone ().unwrap_or (String::from ("")));
                as_labelled_graph_internal (&mut sg, subgraphs, collections::VecDeque::from (s.1.clone ()))?;
                //debug! ("subgraph: {:?}", sg);
                subgraphs.push (sg);
            }
        }
    }
    Ok (())
}

pub fn as_labelled_graphs (parsed_dot: (Option<String>, GraphType, Option<String>, Vec<Statement>))
    -> Result<Vec<graph::graph::LabelledGraph>, error::GraphDotError>
{
    let mut r = Vec::<graph::graph::LabelledGraph>::new ();
    let mut g = graph::graph::LabelledGraph::new_with_name (&parsed_dot.2.unwrap_or (String::from ("")));
    as_labelled_graph_internal (&mut g, &mut r, collections::VecDeque::from (parsed_dot.3))?;

    //debug! ("subgraphs: {:?}", r);
    r.push (g);

    Ok (r)
}

