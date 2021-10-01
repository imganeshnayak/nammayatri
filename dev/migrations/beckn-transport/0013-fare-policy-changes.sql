
CREATE TABLE atlas_transporter.fare_policy_extra_km_rate (
    id character(36) PRIMARY KEY NOT NULL,
    vehicle_variant character varying(255) NOT NULL,
    organization_id character(36) NOT NULL REFERENCES atlas_transporter.organization (id) ON DELETE CASCADE,
    from_extra_distance double precision NOT NULL,
    extra_fare double precision NOT NULL,
    CONSTRAINT fare_policy_extra_km_rate_unique_from_extra_distance UNIQUE (vehicle_variant, organization_id, from_extra_distance)
);

INSERT INTO atlas_transporter.fare_policy_extra_km_rate (
    SELECT T1.id, 
           T1.vehicle_variant,
           T1.organization_id,
           0,
           T1.per_extra_km_rate
    FROM atlas_transporter.fare_policy AS T1);

ALTER TABLE atlas_transporter.fare_policy DROP COLUMN per_extra_km_rate;