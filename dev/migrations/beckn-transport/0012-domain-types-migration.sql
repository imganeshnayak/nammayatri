DELETE FROM atlas_transporter.product_instance AS T1 WHERE T1.type = 'LOCATIONTRACKER';

UPDATE atlas_transporter.product_instance AS T1 
	SET case_id = (SELECT case_id FROM atlas_transporter.product_instance AS T2 WHERE T2.id = T1.parent_id)
	WHERE T1.type <> 'RIDESEARCH';

DELETE FROM atlas_transporter."case" AS T1 WHERE T1.type <> 'RIDESEARCH';

ALTER TABLE atlas_transporter."case" RENAME TO search_request;

ALTER TABLE atlas_transporter.product_instance RENAME COLUMN case_id TO request_id;

ALTER TABLE atlas_transporter.search_request DROP COLUMN parent_case_id;

CREATE TABLE atlas_transporter.ride (
    id character(36) NOT NULL,
    request_id character varying(255) NOT NULL,
    product_id character varying(255) NOT NULL,
    person_id character varying(255),
    person_updated_at timestamp with time zone,
    short_id character varying(36) NOT NULL,
    entity_id character varying(255),
    entity_type character varying(255) NOT NULL,
    quantity bigint NOT NULL,
    price numeric(30,10),
    actual_price double precision,
    status character varying(255) NOT NULL,
    start_time timestamp with time zone NOT NULL,
    end_time timestamp with time zone,
    valid_till timestamp with time zone NOT NULL,
    from_location_id character varying(255),
    to_location_id character varying(255),
    organization_id character varying(255) NOT NULL,
    product_instance_id character varying(255),
    distance double precision NOT NULL DEFAULT 0,
    info text,
    udf1 character varying(255),
    udf2 character varying(255),
    udf3 character varying(255),
    udf4 character varying(255),
    udf5 character varying(255),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

ALTER TABLE atlas_transporter.ride OWNER TO atlas;

ALTER TABLE ONLY atlas_transporter.ride
    ADD CONSTRAINT idx_16395_primary PRIMARY KEY (id);

CREATE INDEX idx_16395_case_id ON atlas_transporter.ride USING btree (request_id);

CREATE INDEX idx_16395_entity_id ON atlas_transporter.ride USING btree (entity_id);

CREATE INDEX idx_16395_organization_id ON atlas_transporter.ride USING btree (organization_id);

CREATE INDEX idx_16395_product_instance_id ON atlas_transporter.ride USING btree (product_instance_id);

CREATE INDEX idx_16395_person_id ON atlas_transporter.ride USING btree (person_id);

CREATE INDEX idx_16395_product_id ON atlas_transporter.ride USING btree (product_id);

CREATE INDEX idx_16395_status ON atlas_transporter.ride USING btree (status);

ALTER TABLE atlas_transporter.rating RENAME COLUMN product_instance_id TO ride_id;

INSERT INTO atlas_transporter.ride 
    SELECT id,
    request_id,
    product_id,
    person_id,
    person_updated_at,
    short_id,
    entity_id,
    entity_type,
    quantity,
    price,
    actual_price,
    status,
    start_time,
    end_time,
    valid_till,
    from_location_id,
    to_location_id,
    organization_id,
    parent_id AS product_instance_id,
    distance,
    info,
    udf1,
    udf2,
    udf3,
    udf4,
    udf5,
    created_at ,
    updated_at FROM atlas_transporter.product_instance AS T1
        WHERE T1.type = 'RIDEORDER';

ALTER TABLE atlas_transporter.ride_cancellation_reason
   DROP CONSTRAINT ride_cancellation_reason_ride_id_fkey
 , ADD  CONSTRAINT ride_cancellation_reason_ride_id_fkey FOREIGN KEY (ride_id)
      REFERENCES atlas_transporter.ride (id) on delete cascade;

ALTER TABLE atlas_transporter.ride_request
   DROP CONSTRAINT ride_request_ride_id_fkey
 , ADD  CONSTRAINT ride_request_ride_id_fkey FOREIGN KEY (ride_id)
      REFERENCES atlas_transporter.ride (id) on delete cascade;

ALTER TABLE atlas_transporter.notification_status
   DROP CONSTRAINT notification_status_ride_id_fkey
 , ADD  CONSTRAINT notification_status_ride_id_fkey FOREIGN KEY (ride_id)
      REFERENCES atlas_transporter.ride (id) on delete cascade;

DELETE FROM atlas_transporter.product_instance AS T1 WHERE T1.type = 'RIDEORDER';

ALTER TABLE atlas_transporter.search_request DROP COLUMN type;

ALTER TABLE atlas_transporter.product_instance DROP COLUMN type;

ALTER TABLE atlas_transporter.product_instance DROP COLUMN parent_id;

ALTER TABLE atlas_transporter.product_instance RENAME TO quote;

ALTER TABLE atlas_transporter.ride RENAME COLUMN product_instance_id TO quote_id;

ALTER TABLE atlas_transporter.search_request DROP COLUMN name;
ALTER TABLE atlas_transporter.search_request DROP COLUMN description;
ALTER TABLE atlas_transporter.search_request DROP COLUMN status;
ALTER TABLE atlas_transporter.search_request DROP COLUMN industry;
ALTER TABLE atlas_transporter.search_request DROP COLUMN end_time;
ALTER TABLE atlas_transporter.search_request DROP COLUMN exchange_type;
ALTER TABLE atlas_transporter.search_request DROP COLUMN provider_type;
ALTER TABLE atlas_transporter.search_request DROP COLUMN requestor_type;
ALTER TABLE atlas_transporter.search_request DROP COLUMN udf2;
ALTER TABLE atlas_transporter.search_request DROP COLUMN udf3;
ALTER TABLE atlas_transporter.search_request DROP COLUMN udf4;
ALTER TABLE atlas_transporter.search_request DROP COLUMN info;
ALTER TABLE atlas_transporter.search_request DROP COLUMN updated_at;

ALTER TABLE atlas_transporter.search_request RENAME COLUMN udf1 TO vehicle_variant;
ALTER TABLE atlas_transporter.search_request RENAME COLUMN udf5 TO bap_id;
ALTER TABLE atlas_transporter.search_request RENAME COLUMN requestor TO requestor_id;
ALTER TABLE atlas_transporter.search_request RENAME COLUMN provider TO provider_id;

ALTER TABLE atlas_transporter.search_request ADD COLUMN transaction_id character(36);
WITH txnIdsTable AS (SELECT id, regexp_split_to_array(T3.short_id, E'_') AS txn_id FROM atlas_transporter.search_request AS T3)
UPDATE atlas_transporter.search_request AS T1 
	SET transaction_id = (SELECT txn_id[2] FROM txnIdsTable AS T2 WHERE T2.id = T1.id);
ALTER TABLE atlas_transporter.search_request DROP COLUMN short_id;
ALTER TABLE atlas_transporter.search_request ADD COLUMN bap_uri character varying(255);

UPDATE atlas_transporter.search_request AS T1 
	SET requestor_id = 'UNKNOWN' WHERE requestor_id IS NULL;
UPDATE atlas_transporter.search_request AS T1 
	SET provider_id = 'UNKNOWN' WHERE provider_id IS NULL;
UPDATE atlas_transporter.search_request AS T1 
	SET bap_id = 'UNKNOWN' WHERE bap_id IS NULL;
UPDATE atlas_transporter.search_request AS T1 
	SET bap_uri = 'UNKNOWN' WHERE bap_uri IS NULL;

ALTER TABLE atlas_transporter.search_request ALTER COLUMN transaction_id SET NOT NULL;
ALTER TABLE atlas_transporter.search_request ALTER COLUMN requestor_id SET NOT NULL;
ALTER TABLE atlas_transporter.search_request ALTER COLUMN provider_id SET NOT NULL;
ALTER TABLE atlas_transporter.search_request ALTER COLUMN bap_id SET NOT NULL;
ALTER TABLE atlas_transporter.search_request ALTER COLUMN bap_uri SET NOT NULL;

ALTER TABLE atlas_transporter.ride RENAME TO old_ride;

CREATE TABLE atlas_transporter.ride_booking (
    id character(36) PRIMARY KEY NOT NULL,
    transaction_id character(36) NOT NULL,
    request_id character(36) NOT NULL REFERENCES atlas_transporter.search_request (id) on delete cascade,
    quote_id character(36) NOT NULL REFERENCES atlas_transporter.quote (id) on delete cascade,
    status character varying(255) NOT NULL,
    provider_id character(36) NOT NULL REFERENCES atlas_transporter.organization (id) on delete cascade,
    bap_id character varying(255) NOT NULL,
    start_time timestamp with time zone NOT NULL,
    requestor_id character(36) NOT NULL,
    from_location_id character(36) NOT NULL REFERENCES atlas_transporter.search_request_location (id) on delete cascade,
    to_location_id character(36) NOT NULL REFERENCES atlas_transporter.search_request_location (id) on delete cascade,
    price character varying(255) NOT NULL,
    distance float NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

CREATE TABLE atlas_transporter.ride (
    id character(36) PRIMARY KEY NOT NULL,
    booking_id character(36) NOT NULL REFERENCES atlas_transporter.ride_booking (id) on delete cascade,
    short_id character varying(36) NOT NULL,
    status character varying(255) NOT NULL,
    driver_id character(36) NOT NULL REFERENCES atlas_transporter.person (id) on delete cascade,
    vehicle_id character(36) NOT NULL REFERENCES atlas_transporter.vehicle (id) on delete cascade,
    otp character(4) NOT NULL,
    tracking_url character varying(255) NOT NULL,
    final_price character varying(255) NOT NULL,
    final_distance float NOT NULL,
    final_location_id character(36) NOT NULL REFERENCES atlas_transporter.search_request_location (id) on delete cascade,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

ALTER TABLE atlas_transporter.quote ALTER COLUMN price SET NOT NULL;
ALTER TABLE atlas_transporter.quote RENAME COLUMN organization_id TO provider_id;
ALTER TABLE atlas_transporter.quote ADD COLUMN distance_to_nearest_driver float;

UPDATE atlas_transporter.quote AS T1 
	SET distance_to_nearest_driver = CAST (udf1 AS float);
UPDATE atlas_transporter.quote AS T1 
	SET distance_to_nearest_driver = 0 WHERE distance_to_nearest_driver IS NULL;

ALTER TABLE atlas_transporter.quote ALTER COLUMN distance_to_nearest_driver SET NOT NULL;
ALTER TABLE atlas_transporter.quote ALTER COLUMN price SET NOT NULL;