-- oracle

create table postinfo( postid int
                     , postdate timestamp
                     , posttopic clob);
create table postcontent( postid int
                        , content clob);
create table postviews( postid int
                      , views int);

insert into postinfo values(1,  TO_TIMESTAMP('2014-11-20 10:00:00', 'YYYY-MM-DD hh24:mi:ss'), 'topic1');
insert into postinfo values(2,  TO_TIMESTAMP('2014-11-20 10:01:00', 'YYYY-MM-DD hh24:mi:ss'), 'topic2');
insert into postinfo values(3,  TO_TIMESTAMP('2014-11-20 10:02:00', 'YYYY-MM-DD hh24:mi:ss'), 'topic3');
insert into postinfo values(4,  TO_TIMESTAMP('2014-11-20 10:03:00', 'YYYY-MM-DD hh24:mi:ss'), 'topic1');
insert into postinfo values(5,  TO_TIMESTAMP('2014-11-20 10:04:00', 'YYYY-MM-DD hh24:mi:ss'), 'topic2');
insert into postinfo values(6,  TO_TIMESTAMP('2014-11-20 10:05:00', 'YYYY-MM-DD hh24:mi:ss'), 'topic3');
insert into postinfo values(7,  TO_TIMESTAMP('2014-11-20 10:06:00', 'YYYY-MM-DD hh24:mi:ss'), 'topic1');
insert into postinfo values(8,  TO_TIMESTAMP('2014-11-20 10:07:00', 'YYYY-MM-DD hh24:mi:ss'), 'topic2');
insert into postinfo values(9,  TO_TIMESTAMP('2014-11-20 10:08:00', 'YYYY-MM-DD hh24:mi:ss'), 'topic3');
insert into postinfo values(10, TO_TIMESTAMP('2014-11-20 10:09:00', 'YYYY-MM-DD hh24:mi:ss'), 'topic1');
insert into postinfo values(11, TO_TIMESTAMP('2014-11-20 10:10:00', 'YYYY-MM-DD hh24:mi:ss'), 'topic2');
insert into postinfo values(12, TO_TIMESTAMP('2014-11-20 10:11:00', 'YYYY-MM-DD hh24:mi:ss'), 'topic3');

insert into postcontent values(1,  'example content 1');
insert into postcontent values(2,  'example content 2');
insert into postcontent values(3,  'example content 3');
insert into postcontent values(4,  'example content 4');
insert into postcontent values(5,  'example content 5');
insert into postcontent values(6,  'example content 6');
insert into postcontent values(7,  'example content 7');
insert into postcontent values(8,  'example content 8');
insert into postcontent values(9,  'example content 9');
insert into postcontent values(10, 'example content 10');
insert into postcontent values(11, 'example content 11');
insert into postcontent values(12, 'example content 12');

insert into postviews values(1,  1200);
insert into postviews values(2,  1100);
insert into postviews values(3,  1000);
insert into postviews values(4,  900);
insert into postviews values(5,  800);
insert into postviews values(6,  700);
insert into postviews values(7,  600);
insert into postviews values(8,  500);
insert into postviews values(9,  400);
insert into postviews values(10, 300);
insert into postviews values(11, 200);
insert into postviews values(12, 100);

