map_size(60, 34).
exit_pos(50, 32).
spawn_pos(30, 2). % Player start position

% health_zone/4 defines single-use health packages
% health_zone(46, 14, 56, 20). % Will spawn package at 46, 14
% health_zone(6, 26).  % Will spawn package at 6, 26

% Random health spawn area: health_spawn_area(XMin, XMax, YMin, YMax, Count)
health_spawn_area(5, 17, 25, 31, 4).
health_spawn_area(46, 57, 23, 25, 2).

% Random equipment spawn area: equipment_spawn_area(Type, XMin, XMax, YMin, YMax, Count)
equipment_spawn_area(sword, 0, 23, 23, 34, 1).
equipment_spawn_area(knife, 0, 23, 23, 34, 2).
equipment_spawn_area(sword, 57, 59, 31, 33, 1).
equipment_spawn_area(knife, 51, 59, 27, 29, 2).
equipment_spawn_area(sword, 42, 59, 0, 22, 2).
equipment_spawn_area(knife, 42, 59, 0, 22, 4).

% Map segments (Walls)
% Above spawn point
map_segment([22, 4], [22, 6]).
map_segment([22, 6], [38, 6]).
map_segment([38, 6], [38, 4]).

% Boss room
map_segment([24, 14], [24, 10]).
map_segment([24, 10], [28, 10]).
map_segment([32, 10], [36, 10]).
map_segment([36, 10], [36, 14]).
map_segment([36, 18], [36, 22]).
map_segment([36, 22], [24, 22]).
map_segment([24, 22], [24, 18]).

% Bottom-right graveyard
map_segment([45, 3], [45, 10]).
map_segment([49, 3], [49, 11]).
map_segment([53, 3], [53, 11]).
map_segment([57, 3], [57, 11]).

map_segment([45, 14], [45, 20]).
map_segment([49, 14], [49, 20]).
map_segment([53, 14], [53, 20]).
map_segment([57, 14], [57, 20]).

% Bottom-left room
map_segment([16, 12], [4, 12]).
map_segment([4, 4], [16, 4]).

map_segment([4, 12], [4, 4]).
% map_segment([16, 4], [16, 12]).
map_segment([16, 4], [16, 6]).
map_segment([16, 10], [16, 12]).

map_segment([7, 6], [13, 6]).
map_segment([7, 10], [13, 10]).

% Middle-left room (Timid Watched Arena)
map_segment([4, 22], [18, 22]).
map_segment([18, 16], [4, 16]).

% map_segment([18, 22], [18, 16]).
map_segment([4, 16], [4, 22]).

map_segment([7, 20], [9, 20]).
map_segment([13, 18], [15, 18]).

% Top-middle room
map_segment([24, 26], [24, 34]).
map_segment([36, 26], [36, 28]).
map_segment([36, 32], [36, 34]).

map_segment([24, 26], [36, 26]).


% Top-right enclosed area
map_segment([56, 34], [56, 30]).
map_segment([56, 30], [50, 30]).
map_segment([50, 30], [50, 26]).

% map_segment([50, 26], [60, 26]).
map_segment([50, 26], [54, 26]).
map_segment([58, 26], [60, 26]).

% Around
map_segment([0, 0], [0, 34]).
map_segment([0, 34], [60, 34]).
map_segment([60, 34], [60, 0]).
map_segment([60, 0], [0, 0]).
