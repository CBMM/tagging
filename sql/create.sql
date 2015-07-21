DROP TABLE IF EXISTS stimulus;
DROP TABLE IF EXISTS stimSet;
DROP TABLE IF EXISTS stimSeq;
DROP TABLE IF EXISTS taggingUser;
DROP TABLE IF EXISTS response;


CREATE TABLE stimSet (
	id uuid primary key DEFAULT uuid_generate_v4(),
	name varchar(400),
	description text,
	baseUrl varchar(500)
);

CREATE TABLE stimulus (
	id uuid primary key DEFAULT uuid_generate_v4(),
	stimSet uuid references stimSet(id) NOT NULL,
	urlSuffix varchar(500) UNIQUE NOT NULL,
	mimeType varchar (200)
);

CREATE TABLE stimSeq (
	id uuid primary key DEFAULT uuid_generate_v4(),
	seqId uuid DEFAULT uuid_generate_v4(),
	seqIndex uuid,
	stimId uuid references stimulus (id),
	UNIQUE (seqId, seqIndex),
	responseType varchar(50)
);

CREATE TABLE taggingUser (
	id uuid primary key DEFAULT uuid_generate_v4(),
	currentStim uuid references stimSeq(id),
	name varchar(200),
	studentId varchar(50),
	roles json
);

CREATE TABLE response (
	taggingUser uuid references taggingUser(id),
	stim uuid references stimSeq(id),
	deliveredTime timestamp,
	respondedTime timestamp,
	response json
);
