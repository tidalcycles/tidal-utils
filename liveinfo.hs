import Sound.Tidal.Tempo

tpb = 12

main = clockedTick tpb onTick

vis steps n = "["
              ++ (map (\x -> if (x == step) then 'X' else '-') [0 .. steps-1])
              ++ "]"
  where step = (n `mod` tpb) `div` (tpb `div` steps)

onTick tempo tick =
  putStr ("cps: " ++ show (cps tempo) ++ " "
          ++ (vis 4 tick)
          ++ " "
          ++ (vis 4 (tick `div` 4))
          ++ " "
          ++ (vis 3 tick)
          ++ " "
          ++ (vis 3 (tick `div` 3))
          ++ " cycle: " ++ show (tick `div` tpb)
          ++ "   \r"
         )
  
