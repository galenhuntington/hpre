foo :: Int -> Bool
''  0 = not $ let once True = let twice False = False
                                  ''    _     = True
                              in once $ twice True
                  ''   _    = False
              in once True
''  x = True
