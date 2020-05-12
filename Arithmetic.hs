module Arith where

data Expression
  = ETrue
  | EFalse
  | EIf Expression Expression Expression
  | EZero
  | ESucc Expression
  | EPred Expression
  | EIsZero Expression

isNumeric :: Expression -> Bool
isNumeric (ESucc t) = isNumeric t
isNumeric EZero = True
isNumeric _ = False

isVal :: Expression -> Bool
isVal ETrue = True
isVal EFalse = True
isVal t
  | isNumeric t = True
  | otherwise = False

evalBigStep :: Expression -> Expression
evalBigStep (EIf ETrue then' else') = evalBigStep then'
evalBigStep (EIf EFalse then' else') = evalBigStep else'
evalBigStep (EIf condition then' else') =
  evalBigStep $ EIf (evalBigStep condition) then' else'
evalBigStep (ESucc t) = ESucc $ evalBigStep t
evalBigStep (EPred EZero) = EZero
evalBigStep t@(EPred (ESucc nv)) =
  if isNumeric nv
    then nv
    else t
evalBigStep (EPred t) = EPred $ evalBigStep t
evalBigStep (EIsZero t)
  | isNumeric t =
    case t of
      EZero -> ETrue
      _ -> EFalse
  | otherwise = EIsZero $ evalBigStep t

eval1 :: Expression -> Maybe Expression
eval1 (EIf ETrue then' else') = Just then'
eval1 (EIf EFalse then' else') = Just else'
eval1 (EIf cond then' else') = do
  cond' <- eval1 cond
  return $ EIf cond' then' else'
eval1 (ESucc t) = do
  t' <- eval1 t
  return $ ESucc t'
eval1 (EPred t)
  | isNumeric t =
    case t of
      EZero -> Just EZero
      ESucc t1 -> Just t1
  | otherwise = do
    t' <- eval1 t
    return $ EPred t'
eval1 (EIsZero t)
  | isNumeric t =
    case t of
      EZero -> Just ETrue
      _ -> Just EFalse
  | otherwise = do
    t' <- eval1 t
    return $ EIsZero t'
eval1 _ = Nothing

eval :: Expression -> Expression
eval t =
  case eval1 t of
    Just t' -> eval t'
    Nothing -> t
