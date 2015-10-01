namespace CompilerTypes
open System
type FunctionPrototype = { Returns:bool; Name:String; Parameters:(String list) }
type FunctionDeclaration = { Name:String; Body:String; AcumulatorCount:int; MaxAcumulatorCount:int; Prototypes:(FunctionPrototype list); LabelCount:int;Returns:bool; } override this.ToString() = this.Name