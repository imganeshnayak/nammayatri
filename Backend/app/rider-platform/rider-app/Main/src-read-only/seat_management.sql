CREATE TABLE seat_management ();

ALTER TABLE seat_management ADD COLUMN blocked integer NOT NULL;
ALTER TABLE seat_management ADD COLUMN booked integer NOT NULL;
ALTER TABLE seat_management ADD COLUMN date date NOT NULL;
ALTER TABLE seat_management ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE seat_management ADD COLUMN ticket_service_category_id character varying(36) NOT NULL;
ALTER TABLE seat_management ADD PRIMARY KEY ( id);