flactags
=====

An OTP application for get metadata information from FLAC files.

Build
-----

    $ rebar compile

or by rebar3

    $ rebar3 compile

Samples
-----
Correct File
```erlang
1> flactags:get_tags("/home/some/Rodrigo Amarante/2013 - Cavalo/09 - Cavalo.flac").
{ok,#{4 => #{<<"ALBUM">> => <<"Cavalo">>,
        <<"ARTIST">> => <<"Rodrigo Amarante">>,
        <<"DATE">> => <<"2013">>,
        <<"GENRE">> => <<"Rock Alternativo">>,
        <<"TITLE">> => <<"Cavalo">>,
        <<"TRACKNUMBER">> => <<"9">>,
        <<"VENDORNAME">> => <<"reference libFLAC 1.3.0 20130526">>}}}
2> 
```
Broken File:
```erlang
2> flactags:get_tags("/home/some/Anna Maria Jopek/2008 - Upojenie (with Pet Metheny)/11 - Letter From Home.flac").
{error,file_not_flac}
3>
```
