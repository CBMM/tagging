alter table assignment add column a_start bigint;
update assignment set a_start = 1;
alter table assignment add column a_end bigint;
update assignment set a_end = 2621 where a_sequence = 5;
update assignment set a_end = 1987 where a_sequence = 4;
alter table assignment alter COLUMN a_start set not null;
alter table assignment alter COLUMN a_end set not null;
