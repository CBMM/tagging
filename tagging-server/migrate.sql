drop table "TaggingUser";

create table assignments (
  a_user bigint references tagging_user (tu_id),
  a_sequence bigint references stimulus_sequence (id),
  a_index bigint
);

alter table stimulus_response add column sr_sequence bigint references stimulus_sequence(id);
update stimulus_response set sr_sequence = 4;
alter table stimulus_response alter column sr_sequence set not null;
alter table stimulus_response add column sr_index bigint;
update stimulus_response set sr_index = 10;
alter table stimulus_response alter column sr_index set not null;

alter table stimulus_request add column sreq_sequence bigint references stimulus_sequence(id);
update stimulus_request set sreq_sequence = 4;
alter table stimulus_request alter column sreq_sequence set not null;
alter table stimulus_request add column sreq_index bigint;
update stimulus_request set sreq_index = 10;
alter table stimulus_request alter column sreq_index set not null;

