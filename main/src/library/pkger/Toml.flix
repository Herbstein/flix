namespace Packager/Toml {

    pub enum InnerToml {
        case TableVal(Map[String, InnerToml]),
        case ArrayVal(List[Map[String, InnerToml]]),
        case StringVal(String),
        case DateVal(String),
        case FloatVal(String),
        case BoolVal(Bool),
        case IntVal(Int)
    }
}
