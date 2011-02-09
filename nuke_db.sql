-- This is highly destructive.
-- Running it looks like
-- $ psql rbv -U vegans -f nuke_db.sql

drop table ticket cascade;
drop table reminder cascade;
drop table usr cascade;
drop table inbox cascade;


create table usr (
       uid serial               primary key,
       email varchar(64)        not null,
       s_and_h char(60)         , /* salt+hash; users can be cookie-only, though */
       unique(email)
);
create index usr_by_email on usr(email);

create table ticket (
       uid serial               not null references usr,
       secret int8              not null
);

create table reminder (
       rid serial               primary key,
       uid serial               not null references usr,
       --subject text           not null,
       message text             not null,
       when_to_send timestamp   not null,
       sent boolean             not null
);
create index reminder_by_when_to_send on reminder(when_to_send);

create table inbox (
       mid serial               primary key,
       subject text             not null, /* Subject: */
       from_address varchar(64) not null, /* From: */
       original_to varchar(128) not null, /* X-Original-To: */
       nominal_dt timestamp     not null, /* Date: */
       auto_submitted boolean   not null, /* Auto-Submitted: */
       received_dt timestamp    not null default now(),
       headers text             not null, 
       body text                not null
);
create index inbox_by_received_dt on inbox(received_dt);