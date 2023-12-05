CREATE TABLE atlas_app.ticket_place ();

ALTER TABLE atlas_app.ticket_place ADD COLUMN close_timings time without time zone ;
ALTER TABLE atlas_app.ticket_place ADD COLUMN description text ;
ALTER TABLE atlas_app.ticket_place ADD COLUMN gallery text[] NOT NULL;
ALTER TABLE atlas_app.ticket_place ADD COLUMN icon_url text ;
ALTER TABLE atlas_app.ticket_place ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_place ADD COLUMN lat double precision ;
ALTER TABLE atlas_app.ticket_place ADD COLUMN lon double precision ;
ALTER TABLE atlas_app.ticket_place ADD COLUMN map_image_url text ;
ALTER TABLE atlas_app.ticket_place ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.ticket_place ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_app.ticket_place ADD COLUMN open_timings time without time zone ;
ALTER TABLE atlas_app.ticket_place ADD COLUMN place_type text ;
ALTER TABLE atlas_app.ticket_place ADD COLUMN short_desc text NOT NULL;
ALTER TABLE atlas_app.ticket_place ADD COLUMN terms_and_conditions text[] NOT NULL;
ALTER TABLE atlas_app.ticket_place ADD PRIMARY KEY ( id);