
use graph;
use std::io;
use thiserror::Error;


#[derive(Error, Debug)]
pub enum GraphDotError
{
    #[error("Edge error: {0}")]
    EdgeError (String),

    #[error("Graph error: {0}")]
    GraphError (String),

    #[error("IO error: {0}")]
    IOError (String),

    #[error("Parse error: {0}")]
    ParseError (String)
}

impl From<io::Error> for GraphDotError
{
    fn from (err: io::Error)
    -> GraphDotError
    {
        GraphDotError::IOError (err.to_string ())
    }
}

impl From<graph::error::GraphError> for GraphDotError
{
    fn from (err: graph::error::GraphError)
    -> GraphDotError
    {
        GraphDotError::GraphError (err.to_string ())
    }
}

impl From<nom::Err<nom::error::Error<&str>>> for GraphDotError
{
    fn from (err: nom::Err<nom::error::Error<&str>>)
        -> GraphDotError
    {
        GraphDotError::ParseError (err.to_string ())
    }
}

