# erlsrcds
Library for performing server queries towards Source Dedicated Servers.

This project was created for educational purposes, but should function properly.
If there are parts of the code which has not optimal implementations, please let me know :). I have tested it on Counter-Strike: Global Offensive and Counter-Strike: Source.

## Usage
The library allows you to query a server and get information back in form of a map or a list.

### Information
To fetch information from the server:

    erlsrcds:info("193.202.115.74", 27127).
        #{"appid" => 730,
        "dedicated" => "d",
        "edf" => 49,
        "gamedesc" => "Counter-Strike: Global Offensive",
        "gamedir" => "csgo",
        "hostname" => "Recess.no #06 Arena 3 Multi-1v1 [ 128 Tick | NOR | Ranking ]",
        "map" => "am_aztec2",
        "maxplayers" => 20,
        "numbots" => 0,
        "numplayers" => 3,
        "os" => "l",
        "passworded" => 0,
        "secure" => 1}

The information is returned as a map.


### Players
To fetch players from the server:

    erlsrcds:player("193.202.115.74", 27125).
        [#{"index" => 0,
           "kill" => 8,
           "name" => "NeXT /T/",
           "time" => 853.4295654296875},
         #{"index" => 0,"kill" => 9,"name" => "VANGUARD","time" => 1222.013427734375},
         #{"index" => 0,"kill" => 6,"name" => "Mucto","time" => 1268.7869873046875},
         #{"index" => 0,
           "kill" => 8,
           "name" => [224,185,150,83,112,114,97,121,32,110,39,32,80,114,97,121,
            32,47,47,32,64,104|...],
           "time" => 1520.8057861328125},
         #{"index" => 0,"kill" => 3,"name" => "Hunter","time" => 2149.594970703125},
         #{"index" => 0,"kill" => 6,"name" => "Bystman","time" => 2876.09130859375},
         #{"index" => 0,
           "kill" => 0,
           "name" => "SUPER BUCK winaskin.com",
           "time" => 4056.263916015625}]

The players are returned as list with maps.

### Rules
To fetch rules from the server:

    erlsrcds:rules("193.202.115.74", 27015).
        #{"sbchecker_version" => "SB-1.5.2F",
          "sm_advertisements_version" => "0.6",
          "mp_friendlyfire" => "0",
          "mp_holiday_nogifts" => "0",
          "mp_forceautoteam" => "0",
          "mp_teamplay" => "0",
          "bot_quota" => "6",
          "mpbhops_enable" => "0",
          "mpbhops_color" => "0",
          "mp_hostagepenalty" => "13",
          "mp_autocrosshair" => "1",
          "r_JeepViewDampenFreq" => "7.0",
          "mp_winlimit" => "0",
          "r_AirboatViewDampenDamp" => "1.0",
          "mp_weaponstay" => "0",
          "mp_teamlist" => "hgrunt;scientist",
          "mp_autoteambalance" => "0",
          "mp_startmoney" => "1337",
          "mp_tournament" => "0",
          "slopefix_enable" => "1",
          "mp_timelimit" => "60",
          "mp_scrambleteams_auto_windifference" => "2",
          "mp_freezetime" => "1",
          "mboosterfix_version" => "0.9.1",
          "mp_flashlight" => "1",
          "mp_stalemate_enable" => "0",
          "r_JeepViewDampenDamp" => "1.0",
          "deathmatch" => "1",
          [...] => [...],...}

The rules are returned as a map.