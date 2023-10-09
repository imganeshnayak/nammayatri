ALTER TABLE atlas_app.merchant ADD COLUMN edit_pickup_distance_threshold double precision DEFAULT 100 NOT NULL;
ALTER TABLE atlas_app.ride ADD COLUMN allowed_edit_location_attempts int DEFAULT 2 ;
ALTER TABLE atlas_app.merchant ADD COLUMN driver_distance_threshold_from_pickup double precision DEFAULT 100 NOT NULL;
