namespace BeSharp.Core

type Box<'a when 'a:struct>  =
    val Value :'a
    new (value:byref<'a>) = {
        Value = value
    }
    