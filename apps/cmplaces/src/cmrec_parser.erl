-module(cmrec_parser).
-export([
         load/0,
         load_one/0,
         load/1,
         event/3
        ]).

load() ->
    load("/Users/pedrogutierrez/Documents/recs.xml").

load_one() ->
    load("/Users/pedrogutierrez/Documents/rec.xml").

load(File) ->
    cmxml:parse(File, fun ?MODULE:event/3, #{ state => none,
                                              callback => {cmrec_importer, write},
                                              image => #{ url => none, title => none },
                                              images => [],
                                              items => [],
                                              categories => []
                                            }).

event({startElement, _, "author", _, _}, _, S) ->
    S#{ state => author };

event({startElement, _, "author_email", _, _}, _, #{ state := author }=S) ->
    S#{ state => author_email };

event({characters, Email}, _, #{ state := author_email }=S) ->
    S#{ state => author_email, author => #{ email => Email }};

event({endElement, _, "author_email", _}, _, #{ state := author_email }=S) ->
    S#{ state => author };

event({startElement, _, "author_display_name", _, _}, _, #{ state := author }=S) ->
    S#{ state => author_display_name };

event({characters, Name}, _, #{ state := author_display_name, author := A }=S) ->
    S#{ state => author_display_name, author => A#{ name => Name }};

event({endElement, _, "author_display_name", _}, _, #{ state := author_display_name }=S) ->
    S#{ state => author };

event({startElement, _, "category_nicename", _, _}, _, S) ->
    S#{ state => category_nicename };

event({characters, Cat}, _, #{ state := category_nicename, categories := Cats }=S) ->
    S#{ state => category_nicename, categories => [Cat | Cats ] };

event({endElement, _, "category_nicename", _}, _, #{ state := category_nicename }=S) ->
    S#{ state =>  none };

event({startElement, _, "image", _, _}, _, S) ->
    S#{ state => image };

event({startElement, _, "url", _, _}, _, #{ state := image }=S) ->
    S#{ state => image_url };

event({characters, Url}, _, #{ state := image_url }=S) ->
    S#{ state => image_url, image => #{ url => Url } };

event({endElement, _, "url", _}, _, #{ state := image_url }=S) ->
    S#{ state =>  image };

event({startElement, _, "title", _, _}, _, #{ state := image }=S) ->
    S#{ state => image_title };

event({characters, Title}, _, #{ state := image_title, image := Image }=S) ->
    S#{ state => image_title, image => Image#{ title => Title } };

event({endElement, _, "title", _}, _, #{ state := image_title }=S) ->
    S#{ state =>  image };

event({endElement, _, "image", _}, _, #{ state := image, image := Image, images := Images }=S) ->
    S#{ state =>  none, images => [ Image | Images ] };

event({startElement, _, "item", _, _}, _, #{ state := none }=S) ->
    S#{ state =>  item, item => #{} };

event({startElement, _, "title", _, _}, _, #{ state := item }=S) ->
    S#{ state => item_title };

event({characters, Title}, _, #{ state := item_title, item := Item }=S) ->
    S#{ item => Item#{ title => Title } };

event({endElement, _, "title", _}, _, #{ state := item_title}=S) ->
    S#{ state => item };

event({startElement, _, "post_id", _, _}, _, #{ state := item }=S) ->
    S#{ state => item_id };

event({characters, Id}, _, #{ state := item_id, item := Item }=S) ->
    S#{ item => Item#{ id => Id } };

event({endElement, _, "post_id", _}, _, #{ state := item_id}=S) ->
    S#{ state => item };

event({startElement, _, "post_parent", _, _}, _, #{ state := item }=S) ->
    S#{ state => item_parent };

event({characters, Id}, _, #{ state := item_parent, item := Item }=S) ->
    S#{ item => Item#{ parent => Id } };

event({endElement, _, "post_parent", _}, _, #{ state := item_parent}=S) ->
    S#{ state => item };

event({startElement, _, "post_date", _, _}, _, #{ state := item }=S) ->
    S#{ state => item_date };

event({characters, Date }, _, #{ state := item_date, item := Item }=S) ->
    <<Year:4/binary,"-",Month:2/binary,"-",Day:2/binary," ", Hour:2/binary,":", Min:2/binary,":",Secs:2/binary>> = cmkit:to_bin(Date),
    S#{ item => Item#{ date => {
                         cmkit:bin_to_number(Year),
                         cmkit:bin_to_number(Month),
                         cmkit:bin_to_number(Day),
                         cmkit:bin_to_number(Hour),
                         cmkit:bin_to_number(Min),
                         cmkit:bin_to_number(Secs)
                        }}};

event({endElement, _, "post_date", _}, _, #{ state := item_date}=S) ->
    S#{ state => item };

event({startElement, _, _, {"content", "encoded" }, _}, _, #{ state := item }=S) ->
    S#{ state => item_content };

event({characters, Content }, _, #{ state := item_content, item := Item }=S) ->
    S#{ item => Item#{ content => Content} };

event({endElement, _, _, {"content", "encoded"}}, _, #{ state := item_content }=S) ->
    S#{ state => item };

event({startElement, _, _, {_, "post_type" }, _}, _, #{ state := item }=S) ->
    S#{ state => item_type };

event({characters, Type}, _, #{ state := item_type, item := Item }=S) ->
    S#{ item => Item#{ type => Type } };

event({endElement, _, _, {_, "post_type"}}, _, #{ state := item_type }=S) ->
    S#{ state => item };

event({startElement, _, _, {_, "attachment_url" }, _}, _, #{ state := item }=S) ->
    S#{ state => item_url };

event({characters, Url}, _, #{ state := item_url, item := Item }=S) ->
    S#{ item => Item#{ url => Url } };

event({endElement, _, _, {_, "attachment_url"}}, _, #{ state := item_url }=S) ->
    S#{ state => item };

event({endElement, _, "item", _}, _, #{ callback := {Mod, Fun}, 
                                    state := item, 
                                    item := Item, 
                                    items := Items }=S) ->
    Item2 = parseHtml(Item),
    Mod:Fun(Item2),
    S#{ state =>  none, items => [Item2 | Items] };

event(endDocument, _, #{ items := Items }=Data) ->
    Data2 = maps:without([item, image, state], Data),
    Data2#{ items => lists:reverse(Items) };

event(_Ev, _Loc, Data) ->
    %cmkit:log({xml_event, Ev, Loc, Data}),
    Data.

parseHtml(#{ type := "page", content := Content }=Item) ->
    Html2 = "<div>" ++ Content ++ "</div>",
    Html3 = lists:flatten([io_lib:format("~c", [V]) || V <- Html2]),
    Html4 = findImages(Html3),
    Content2 = trane:sax(Html4, fun htmlToken/2, #{ state => none, category => none, images => [], categories => [] }),
    Content3 = maps:without([category, state], Content2),
    Item#{ content => Content3};

parseHtml(#{ type := _ }=Item) ->
    Item.

findImages(Contents) when is_list(Contents) ->
    Bin = list_to_binary(Contents),
    Bin2 = binary:replace(Bin, <<"[gallery ids=\"">>, <<"<pre>">>, [global]),
    Bin3 = binary:replace(Bin2, <<"\" type=\"rectangular\"]">>, <<"</pre>">>, [global]),
    binary_to_list(Bin3).

htmlToken({tag, "h3", _}, #{ state := none }=Acc)  ->
    Acc#{ state => category };

htmlToken({text, Bin}, #{ state := category }=Acc) ->
    Acc#{ category => #{ name => Bin, contents => none } };

htmlToken({end_tag, "h3"}, Acc)  ->
    Acc#{ state => category_content };

htmlToken({text, Bin}, #{ state := category_content, category := Cat, categories := Cats }=Acc) ->
    Cat2 = Cat#{ contents => Bin },
    Acc#{ state => none, category => none, categories  => [ Cat2 | Cats ] };

htmlToken({tag, "pre", _}, #{ state := none }=Acc)  ->
    Acc#{ state => images };

htmlToken({text, Bin}, #{ state := images }=Acc) ->
    Acc#{ images => lists:map(fun(Id) -> binary_to_list(Id) end, binary:split(Bin, <<",">>, [global])) };

htmlToken({end_tag, "pre"}, Acc)  ->
    Acc#{ state => none };

htmlToken(_, Acc) ->
    Acc.
