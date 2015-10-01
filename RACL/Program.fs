module Compilation

open System

open Microsoft.FSharp.Text.Lexing
open CompilerTypes

open Ast
open Lexer

open Parser



let acumulatorname = "ac"


let rec ReadParameters paramblock =
    match paramblock with
    | FormalParameters paramList -> paramList |> List.map(fun x -> (evalIdentifier x)) 
    | FormalParameterless -> List.Empty                         

and ExtractFunctionPrototype funcdef = 
    match funcdef with
    | FunctionDeclaration(name, parameters, _ ) ->  let namestr = (evalIdentifier name)
                                                    CheckValidIdentifier namestr
                                                    { Returns = true; Name = namestr; Parameters = (ReadParameters parameters); }
    | VoidFunctionDeclaration(name, parameters, _) -> let namestr = (evalIdentifier name)
                                                      CheckValidIdentifier namestr
                                                      { Returns = false; Name = namestr; Parameters = (ReadParameters parameters); }

and evalIdentifier idn = 
    match idn with
    | Identifier i -> i

and evalExpression exp functionDecl =
    match exp with
    | Numeric i -> { functionDecl with Body = String.Concat(functionDecl.Body,"set " + (getAcumulator (functionDecl.AcumulatorCount)) + " = "+ (evalNumber i) + "\n"); AcumulatorCount = functionDecl.AcumulatorCount + 1;}
    | String s -> { functionDecl with Body = String.Concat(functionDecl.Body,"set " + (getAcumulator (functionDecl.AcumulatorCount)) + " = "+ s + "\n"); AcumulatorCount = functionDecl.AcumulatorCount + 1;}
    | ParenEx ex -> evalExpression ex functionDecl
    | ExpVariable var ->
        let (variable, newfunc) = evalVariable var functionDecl
        { newfunc with Body = String.Concat(newfunc.Body, "set " + (getAcumulator (newfunc.AcumulatorCount)) + " = " + variable + "\n"); AcumulatorCount = newfunc.AcumulatorCount + 1;} 
    | PlusSignal ex -> let interm = evalExpression ex functionDecl
                       let oldAcumulator = getAcumulator (interm.AcumulatorCount - 1)
                       { interm with Body = String.Concat(interm.Body, "set " + oldAcumulator + " = +" + oldAcumulator + "\n"); AcumulatorCount = interm.AcumulatorCount; }
    | MinusSignal ex -> let interm = evalExpression ex functionDecl
                        let oldAcumulator = getAcumulator (interm.AcumulatorCount - 1)
                        { interm with Body = String.Concat(interm.Body, "set " + oldAcumulator + " = -" + oldAcumulator + "\n"); AcumulatorCount = interm.AcumulatorCount; }
    | Times(ex1,ex2) -> let inter = evalExpression ex1 functionDecl
                        let leftAcumulator = getAcumulator (inter.AcumulatorCount - 1)
                        let inter2 = evalExpression ex2 inter
                        let rightAcumulator = getAcumulator (inter2.AcumulatorCount - 1)
                        { inter2 with Body = String.Concat(inter2.Body, "set " + rightAcumulator + " = " + leftAcumulator + " * " + rightAcumulator + "\n"); }
    | Divide(ex1,ex2) -> let inter = evalExpression ex1 functionDecl
                         let leftAcumulator = getAcumulator (inter.AcumulatorCount - 1)
                         let inter2 = evalExpression ex2 inter
                         let rightAcumulator = getAcumulator (inter2.AcumulatorCount - 1)
                         { inter2 with Body = String.Concat(inter2.Body, "set " + rightAcumulator + " = " + leftAcumulator + " / " + rightAcumulator + "\n"); }
    | Mod(ex1,ex2) -> let inter = evalExpression ex1 functionDecl
                      let leftAcumulator = getAcumulator (inter.AcumulatorCount - 1)
                      let inter2 = evalExpression ex2 inter
                      let rightAcumulator = getAcumulator (inter2.AcumulatorCount - 1)
                      { inter2 with Body = String.Concat(inter2.Body, "set " + rightAcumulator + " = " + leftAcumulator + " MOD " + rightAcumulator + "\n"); }
    | Plus(ex1,ex2) -> let inter = evalExpression ex1 functionDecl
                       let leftAcumulator = getAcumulator (inter.AcumulatorCount - 1)
                       let inter2 = evalExpression ex2 inter
                       let rightAcumulator = getAcumulator (inter2.AcumulatorCount - 1)
                       { inter2 with Body = String.Concat(inter2.Body, "set " + rightAcumulator + " = " + leftAcumulator + " + " + rightAcumulator + "\n"); }
    | Minus(ex1,ex2) -> let inter = evalExpression ex1 functionDecl
                        let leftAcumulator = getAcumulator (inter.AcumulatorCount - 1)
                        let inter2 = evalExpression ex2 inter
                        let rightAcumulator = getAcumulator (inter2.AcumulatorCount - 1)
                        { inter2 with Body = String.Concat(inter2.Body, "set " + rightAcumulator + " = " + leftAcumulator + " - " + rightAcumulator + "\n"); }
    | FunctionCall(name, parameterBlock) ->
        let ((parameters:string list), newfunc) = evalActualParameterBlock parameterBlock functionDecl
        let stringName = evalIdentifier name
        let prototypelist = newfunc.Prototypes |> List.filter (fun x -> x.Name = stringName && x.Parameters.Length = parameters.Length)
        if(prototypelist.Length = 0) then
           { newfunc with Body = String.Concat(newfunc.Body, (stringName + " "), (if (parameters.Length) = 0 then String.Empty else String.Concat(parameters |> List.map(fun x -> x + " " ))), "\n"); }
        else
            let prototype = prototypelist.[0]
            let declares = String.Concat(prototype.Parameters |> List.map(fun x -> "GLOBAL " + x + "\n"))
            let assigns = if parameters.Length > 0 then String.Concat (List.zip prototype.Parameters parameters |> List.map (fun (x, y) -> "set " + x + " = " + y + "\n")) else String.Empty
            let deletes = String.Concat(List.map (fun x-> "DELVAR " + x + "\n") prototype.Parameters)
            let returnstatement = if prototype.Returns then "set " + (getAcumulator newfunc.AcumulatorCount) + " = RETN\n" else String.Empty
            { newfunc with Body = String.Concat(newfunc.Body, declares, assigns, "GOSUB " + stringName + "\n", deletes, returnstatement); AcumulatorCount = newfunc.AcumulatorCount + 1; }

and evalTopExpression exp functionDecl clear_acumulator =
    let nf = evalExpression exp functionDecl
    let acumulator = getAcumulator (nf.AcumulatorCount - 1)
    let nf2 = {nf with MaxAcumulatorCount = Math.Max(nf.AcumulatorCount, nf.MaxAcumulatorCount); AcumulatorCount = (if clear_acumulator = true then 0 else nf.AcumulatorCount); }
    (acumulator, nf2)

and evalActualParameter param functionDecl =
    match param with
    | ExpressionParameter expr ->
        evalTopExpression expr functionDecl false
    | StringParameter str -> (str, functionDecl)
    | RefIdentifierParameter name -> (evalIdentifier name, functionDecl)
    | RefArrayParameter(name, exp) ->
        let (acum, nf) = evalTopExpression exp functionDecl false
        ((evalIdentifier name) + "[" + acum + "]", nf)


and CreateFunctionParameters (param_list: ActualParameter list) functionDecl counter = 
    let item = param_list.[counter]
    let (acumulator, nf) = evalActualParameter item functionDecl
    if( counter + 1 < param_list.Length) then let (a, b) = CreateFunctionParameters param_list nf (counter + 1)
                                              (acumulator :: a, b)
    else (acumulator :: List.Empty, nf)
    
and evalActualParameterBlock apb functionDecl =
    match apb with
    | ActualParameterless -> (List.Empty, functionDecl)
    | ActualParameters param_list -> 
        let (acum, nf) = CreateFunctionParameters param_list functionDecl 0
        (acum, {nf with AcumulatorCount = 0; })

and evalCond (ex1,ex2,operator) functionDecl =
    let (leftacum, inter) = evalTopExpression ex1 functionDecl false
    let (rightacum, inter2) = evalTopExpression ex2 inter true
    (leftacum + " " + operator + " " + rightacum, inter2)

and evalCondition cond functionDecl =
    evalCond (
        match cond with
        | IsEqual(ex1, ex2) -> (ex1, ex2, "=")
        | IsNotEqual(ex1, ex2) -> (ex1, ex2, "<>") 
        | IsLowerThan(ex1, ex2) -> (ex1, ex2, "<")
        | IsGreaterThan(ex1, ex2) -> (ex1, ex2, ">")
        | IsLowerEqualThan(ex1, ex2) -> (ex1, ex2, "<=")
        | IsGreaterEqualThan(ex1, ex2) -> (ex1, ex2, ">=")
    ) functionDecl  

and evalVariable var functionDecl =
    match var with
    | Variable idn -> ((evalIdentifier idn), functionDecl)
    | Array(idn, exp) -> 
        let (acum, inter) = evalTopExpression exp functionDecl false
        ((evalIdentifier idn) + "[" + acum + "]", inter)
        
and evalNumber nbm =
    match nbm with 
    | Integer i -> i               

and evalDAssignment var value operator functionDecl =
        let (variable, newfunc) = evalVariable var functionDecl
        let (acumulator, newfunc2) = evalTopExpression value newfunc true
        { newfunc2 with Body = String.Concat(newfunc2.Body, "set " + variable + " = " + variable + " " + operator + " " + acumulator + "\n"); }

and evalBodyStatement body functionDecl =
    match body with
    | Assignment(var, value) -> 
        let (variable, newfunc) = evalVariable var functionDecl
        let(acumulator, newfunc2) = evalTopExpression value newfunc true
        { newfunc2 with Body = String.Concat(newfunc2.Body, "set " + variable + " = " + acumulator + "\n"); }
    | PlusAssignment(var, value) -> evalDAssignment var value "+" functionDecl
    | MinusAssignment(var, value) -> evalDAssignment var value "-" functionDecl
    | TimesAssignment(var, value) -> evalDAssignment var value "*" functionDecl
    | DivAssignment(var, value) -> evalDAssignment var value "/" functionDecl
    | ModAssignment(var, value) -> evalDAssignment var value "MOD" functionDecl
    | FieldDeclarationStatement fds -> evalFieldDeclaration fds functionDecl false
    | GlobalFieldDeclaration gfd -> evalFieldDeclaration gfd functionDecl true
    | Return exp ->
        if functionDecl.Returns = false then failwith ("Function \"" + (functionDecl.Name) + "\" cant return values.")
        let (acumulator, inter) = evalTopExpression exp functionDecl true
        { inter with Body = String.Concat(inter.Body, "set RETN = " + acumulator + "\n", "GOTO 0\n"); }
    | Body bsl -> evalBody bsl functionDecl 0
    | EmptyBody -> functionDecl
    | While(cond, bd) ->
        let label = functionDecl.LabelCount.ToString()
        let nf = { functionDecl with Body = String.Concat(functionDecl.Body, "LABEL " + label + "\n"); LabelCount = functionDecl.LabelCount + 1;}
        let (condition, newfunc) = evalCondition cond nf
        let newfunc2 = { newfunc with Body = String.Concat(newfunc.Body, "IF " + condition + "\n"); }
        let newfunc3 = evalBodyStatement bd newfunc2
        { newfunc3 with Body = String.Concat(newfunc3.Body, "GOTO " + label + "\n", "ENDIF \n"); }
    | DoWhile(bd, cond) ->
        let label = functionDecl.LabelCount.ToString()
        let nf = { functionDecl with Body = String.Concat(functionDecl.Body, "LABEL " + label + "\n"); LabelCount = functionDecl.LabelCount + 1;}
        let newfunc = evalBodyStatement bd nf
        let (condition, newfunc2) = evalCondition cond newfunc
        { newfunc2 with Body = String.Concat(newfunc2.Body, "IF " + condition + "\n", "GOTO " + label + "\nENDIF\n"); }
    | If(cond, bd, els) ->
        let (condition, newbody) = evalCondition cond functionDecl
        let newbody2 = { newbody with Body = String.Concat(newbody.Body, "IF " + condition + "\n"); }
        let newbody3 = (evalBodyStatement bd newbody2) |> evalBodyStatement els
        { newbody3 with Body = String.Concat(newbody3.Body, "ENDIF\n"); }
    | ElseIf(cond, bd, els) ->
        let nb1 = { functionDecl with Body = String.Concat(functionDecl.Body, "ELSE\n"); }
        let (condition, newbody) = evalCondition cond nb1
        let newbody2 = { newbody with Body = String.Concat(newbody.Body, "IF " + condition + "\n"); }
        let newbody3 = (evalBodyStatement bd newbody2) |> evalBodyStatement els
        { newbody3 with Body = String.Concat(newbody3.Body, "ENDIF\n"); }
    | Else(bd) -> ({ functionDecl with Body = String.Concat(functionDecl.Body, "ELSE\n"); }) |> evalBodyStatement bd
    | NoElse -> functionDecl
    | BodyFunctionCall cll -> 
        let (acum, func) = evalTopExpression cll functionDecl true
        func
    | For(assign, cond, iter, bd) ->
        let newfunc = evalForAssignmentBlock assign functionDecl
        let label = newfunc.LabelCount.ToString()
        let newfunc2 = { newfunc with Body = String.Concat(newfunc.Body, "LABEL " + label + "\n"); LabelCount = newfunc.LabelCount + 1; }
        let newfunc3 = evalBodyStatement bd newfunc2
        let newfunc4 = evalForIteratorBlock iter newfunc3
        let (condition, newfunc5) = evalForConditionBlock cond newfunc4
        if condition = String.Empty then { newfunc5 with Body = String.Concat(newfunc5.Body, "GOTO "+ label + "\n"); }
        else { newfunc5 with Body = String.Concat(newfunc5.Body, "IF " + condition + "\n", "GOTO "+ label + "\n", "ENDIF\n"); }

and evalForAssignmentBlock fab functionDecl =
    match fab with 
    | ForAssignment(name, exp) ->
        let (acumulator, newfunc) = evalTopExpression exp functionDecl true
        { newfunc with Body = String.Concat(newfunc.Body, "set " + (evalIdentifier name) + " = " + acumulator + "\n"); }
    | EmptyForInitializer -> functionDecl

and evalForConditionBlock fcb functionDecl = 
    match fcb with 
    | ForCondition cond -> evalCondition cond functionDecl
    | EmptyForCondition -> (String.Empty, functionDecl)

and evalForIteratorBlock fib functionDecl = 
    match fib with
    | ForIterator bd -> evalBodyStatement bd functionDecl
    | EmptyForIterator -> functionDecl

and evalFunctionDeclaration funcDecl functionPrototypes =
    match funcDecl with 
    | FunctionDeclaration(name, _ , body) -> 
        let nm = evalIdentifier name
        let func = { Name = nm; Body = ""; AcumulatorCount = 0; Prototypes = functionPrototypes; LabelCount = 1; MaxAcumulatorCount = 0; Returns = true;}
        let func2 = evalBodyStatement body func
        let acumdefines = String.Concat(List.init func2.MaxAcumulatorCount (fun x -> "define " + acumulatorname + x.ToString() + "\n"))
        { func2 with Body = String.Concat("program " + nm + "\n", "GLOBAL RETN\n", acumdefines, func2.Body,"LABEL 0\n", "end\n"); }
    | VoidFunctionDeclaration(name, _ , body) -> 
        let nm = evalIdentifier name
        let func = { Name = nm; Body = ""; AcumulatorCount = 0; Prototypes = functionPrototypes; LabelCount = 0; MaxAcumulatorCount = 0; Returns = false;}
        let func2 = evalBodyStatement body func
        let acumdefines = String.Concat(List.init func2.MaxAcumulatorCount (fun x -> "define " + acumulatorname + x.ToString() + "\n"))
        { func2 with Body = String.Concat("program " + nm + "\n", acumdefines, func2.Body, "end\n"); }

and evalBody bsl functionDecl count =
    let fd = evalBodyStatement bsl.[count] functionDecl
    if (count+1 < bsl.Length) then evalBody bsl fd (count+1)
    else fd

and evalFieldDeclaration fld functionDecl isGlobal =
    match fld with
    | CreateAndAssignVariable(name, exp) ->
        let stringName = evalIdentifier name
        CheckValidIdentifier stringName
        let (acumulator, newfunc) = evalTopExpression exp functionDecl true
        let defineKeyword = if(isGlobal) then "GLOBAL" else "DEFINE"
        { newfunc with Body = String.Concat(newfunc.Body, defineKeyword + " " + stringName + "\n", "set " + stringName + " = " + acumulator + "\n"); }
    | CreateVariable name ->
        let stringName = evalIdentifier name
        CheckValidIdentifier stringName
        let defineKeyword = if(isGlobal) then "GLOBAL" else "DEFINE"
        { functionDecl with Body = String.Concat(functionDecl.Body, defineKeyword + " " + stringName + "\n"); }
    | CreateArray(name, index) ->
        let stringName = evalIdentifier name
        CheckValidIdentifier stringName
        let number = evalNumber index
        let defineKeyword = if(isGlobal) then "DIMG" else "DIM"
        { functionDecl with Body = String.Concat(functionDecl.Body, defineKeyword + " " + stringName + "[" + number + "]" + "\n"); }

and getAcumulator acumCount = acumulatorname + acumCount.ToString()

and CheckValidIdentifier idn = if idn.Length > 5 then failwith ("name too long: " + idn)
    
and evalProgram pr =
    match pr with
    | Program f -> let Prototypes = f |> List.map(fun x -> (ExtractFunctionPrototype x))
                   f |> List.map(fun x -> (evalFunctionDeclaration x Prototypes))


let compile data = 
    let lexbuff = LexBuffer<char>.FromString(data)
    let ast = Parser.start Lexer.tokenize lexbuff
    (evalProgram ast) |> List.toArray 

(*
//let rec readAndProcess() =
//    printf ":"
//    match Console.ReadLine() with
  //  | "quit" -> ()
 //   | expr ->
 //       try
  //          printfn "Lexing [%s]" expr
            let lexbuff = LexBuffer<char>.FromString(expr)
            
            printfn "Parsing..."
            let equation = Parser.start Lexer.tokenize lexbuff
            printfn "Evaluating Equation..."
            let test = evalProgram equation
            let result = ""
            
            printfn "Result: %s" (result.ToString())
            
        with ex ->
            printfn "Unhandled Exception: %s" ex.Message

        readAndProcess()

readAndProcess()
*)

