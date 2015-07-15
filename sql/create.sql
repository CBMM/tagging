DROP TABLE IF EXISTS stimulus;
CREATE TABLE stimulus (
	id int primary key,
	stimSet references stimSet(id) NOT NULL,
	urlSuffix varchar(500) UNIQUE NOT NULL,
	mimeType varchar (200)
);

CREATE TABLE stimSet (
	id int primary key,
	name varchar(400),
	description text,
	baseUrl varchar (500),
);

CREATE TABLE stimSeq (
	seqId int,
	seqIndex int,
	stimId int references stimulus (id),
	UNIQUE (seqId, seqIndex)
	responseType varchar(50)
);

CREATE TABLE user (
	id int primary key,
	currentStimSeqId int references stimSeq(seqId),
	currentStimSeqIndex int references stimSeq(seqIndex),
	name varchar(200),
	studentId varchar(50),
	roles json
);

CREATE TABLE response (
	user int references user(id),
	seqId int references stimSeq(seqId),
	seqIndex int references stimSeq(seqIndex),
	deliveredTime timestamp,
	respondedTime timestamp,
	response json 
);
