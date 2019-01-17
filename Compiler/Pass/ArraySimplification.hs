module Compiler.Pass.ArraySimplification (

) where

simplifyArrays :: Ast -> Ast
simplifyArrays = traverse simplifyFunctionDefinitions

simplifyFunctionDefinitions :: FunctionBody -> FunctionBody
simplifyFunctionDefinitions f@(FunctionDefinition {body = b}) = f {body = simplifyBody}

simplifyBody :: FunctionBody -> FunctionBody
