map_size(60, 34).
exit_pos(50, 32).
spawn_pos(30, 2). % 玩家进入冒险地图的初始位置

% 回到安全屋的门 (可选)
% portal_pos(10, 0, 'safehouse.pl'). 

health_zone(46, 14, 56, 20).
health_zone(6, 26, 16, 32).

map_segment([22, 4], [22, 6]).
map_segment([22, 6], [38, 6]).
map_segment([38, 6], [38, 4]).
map_segment([24, 14], [24, 10]).
map_segment([24, 10], [28, 10]).
map_segment([32, 10], [36, 10]).
map_segment([36, 10], [36, 14]).
map_segment([36, 18], [36, 22]).
map_segment([36, 22], [24, 22]).
map_segment([24, 22], [24, 18]).
map_segment([44, 4], [44, 8]).
map_segment([48, 2], [48, 8]).
map_segment([52, 2], [52, 8]).
map_segment([56, 2], [56, 8]).
map_segment([16, 12], [4, 12]).
map_segment([4, 12], [4, 4]).
map_segment([4, 4], [16, 4]).
map_segment([16, 4], [16, 12]).
map_segment([4, 22], [18, 22]).
map_segment([18, 22], [18, 16]).
map_segment([18, 16], [4, 16]).
map_segment([4, 16], [4, 22]).
map_segment([32, 26], [36, 26]).
map_segment([36, 26], [36, 30]).
map_segment([28, 26], [24, 26]).
map_segment([24, 26], [24, 34]).
map_segment([56, 34], [56, 30]).
map_segment([56, 30], [50, 30]).
map_segment([50, 30], [50, 26]).
map_segment([50, 26], [60, 26]).
map_segment([0, 0], [0, 34]).
map_segment([0, 34], [60, 34]).
map_segment([60, 34], [60, 0]).
map_segment([60, 0], [0, 0]).
