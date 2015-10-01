namespace Ast
open System

type Expression =
    | Numeric of Number
    | String of String
    | ParenEx of Expression
    | ExpVariable of Variable
    | PlusSignal of Expression
    | MinusSignal of Expression
    | Times of Expression * Expression
    | Divide of Expression * Expression
    | Mod of Expression * Expression
    | Plus of Expression * Expression
    | Minus of Expression * Expression
    | FunctionCall of Identifier * ActualParameterBlock

and Variable =
    | Array of Identifier * Expression
    | Variable of Identifier

and Identifier =
    | Identifier of String

and ActualParameterBlock =
    | ActualParameterless
    | ActualParameters of ActualParameter list

and ActualParameter = 
    | ExpressionParameter of Expression
    | StringParameter of String
    | RefIdentifierParameter of Identifier
    | RefArrayParameter of Identifier * Expression

and BodyStatement =
    | Assignment of Variable * Expression
    | PlusAssignment of Variable * Expression
    | MinusAssignment of Variable * Expression
    | TimesAssignment of Variable * Expression
    | DivAssignment of Variable * Expression
    | ModAssignment of Variable * Expression
    | FieldDeclarationStatement of FieldDeclaration
    | GlobalFieldDeclaration of FieldDeclaration
    | Return of Expression
    | Body of BodyStatement list
    | EmptyBody
    | While of Condition * BodyStatement
    | DoWhile of BodyStatement * Condition
    | If of Condition * BodyStatement * BodyStatement
    | ElseIf of Condition * BodyStatement * BodyStatement
    | Else of BodyStatement
    | NoElse
    | For of ForAssignmentBlock * ForConditionBlock * ForIteratorBlock * BodyStatement
    | BodyFunctionCall of Expression

and ForAssignmentBlock = 
    | ForAssignment of Identifier * Expression
    | EmptyForInitializer

and ForConditionBlock =
    | ForCondition of Condition
    | EmptyForCondition

and ForIteratorBlock =
    | ForIterator of BodyStatement
    | EmptyForIterator

and FieldDeclaration =
    | CreateAndAssignVariable of Identifier * Expression
    | CreateVariable of Identifier
    | CreateArray of Identifier * Number

and FunctionDeclarationV =
    | FunctionDeclaration of Identifier * FormalParameterBlock * BodyStatement
    | VoidFunctionDeclaration of Identifier * FormalParameterBlock * BodyStatement

and FormalParameterBlock =
    | FormalParameters of Identifier list
    | FormalParameterless

and Condition =
    | IsEqual of Expression * Expression
    | IsNotEqual of Expression * Expression
    | IsLowerThan of Expression * Expression
    | IsGreaterThan of Expression * Expression
    | IsLowerEqualThan of Expression * Expression
    | IsGreaterEqualThan of Expression * Expression

and Number = 
    | Integer of String

and Program =
    | Program of FunctionDeclarationV list
