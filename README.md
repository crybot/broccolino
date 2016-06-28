<p align="center">
    <img width="250px" height:100px;" src="http://image.flaticon.com/icons/svg/144/144199.svg"/>
</p>

## <strong> Current Grammar </strong>
<p>
<i>
    post-fix F stands for "left Factored" 
</i>
</p>
<p>
<i>
post-fix ' stands for productions from which left recursion has been eliminated
</i>
</p>

```haskell
- S -> DecL | ComL | Expr
- DecL -> Dec DecLF
- DecLF -> and DecL | epsilon
- ComL -> Com ComLF
- ComLF -> ; ComL | epsilon
- Dec -> int ide | int ide = Expr
- Com -> ide = Expr | if Expr then ComL else ComL end
         | while Expr do ComL end
- Expr -> TermExpr' | -TermExpr'
- Expr' -> +TermExpr' | -TermExpr' | epsilon
- Term -> FactTerm'
- Term' -> *FactTerm' | /FactTerm' | epsilon
- Fact -> num | ide | (Expr)

