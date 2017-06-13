cd Server
ghc -o ../Output/server -outputdir bin server.hs

cd ../Client
ghc -o ../Output/client -outputdir bin client.hs