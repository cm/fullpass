-module(cmrec_importer).
-export([write/1]).

write(#{ id := Id, 
         url := Url, 
         title := Title, 
         date := Date,
         type := "attachment" }) ->
    
    BinId = cmkit:to_bin(Id),
    BinUrl = cmkit:to_bin(Url),
    BinTitle = cmkit:to_bin(Title),
    Pkey = {image, BinId},
    Item = #{ id => BinId,
              url => BinUrl,
              title => Title,
              date => Date },
    
    Pairs = [ {{token, cmkit:lower_bin(T)}, Pkey} 
              || T <- binary:split(BinTitle, <<" ">>, [global])],
    cmdb:write(weekonekt, [{Pkey, Item}|Pairs]);

write(Item) -> 
    cmkit:log({cmrec_importer, ignore, Item}). 


