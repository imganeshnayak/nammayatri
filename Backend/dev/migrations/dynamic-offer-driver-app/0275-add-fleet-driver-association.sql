CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.fleet_driver_association(
    id TEXT PRIMARY KEY,
    driver_id TEXT NOT NULL,
    fleet_owner_id TEXT NOT NULL,
    is_active BOOLEAN NOT NULL ,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate ADD COLUMN fleet_owner_id character (36);


----- DROP COLUMN -----

ALTER TABLE atlas_driver_offer_bpp.vehicle DROP COLUMN IF EXISTS fleet_owner_id;