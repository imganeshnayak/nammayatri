ALTER TABLE atlas_app.quote ADD COLUMN provider_url varchar(255);

UPDATE atlas_app.quote
  SET provider_url = org.callback_url
  FROM (SELECT id, callback_url FROM atlas_app.organization) as org
  WHERE quote.provider_id = org.id;

ALTER TABLE atlas_app.quote ALTER COLUMN provider_url SET NOT NULL;
ALTER TABLE atlas_app.quote DROP COLUMN provider_id;

---

ALTER TABLE atlas_app.ride_booking ADD COLUMN provider_url varchar(255);

UPDATE atlas_app.ride_booking
  SET provider_url = quote.provider_url
  FROM (SELECT id, provider_url FROM atlas_app.quote) as quote
  WHERE quote.id = ride_booking.quote_id;

ALTER TABLE atlas_app.ride_booking ALTER COLUMN provider_url SET NOT NULL;
ALTER TABLE atlas_app.ride_booking DROP COLUMN provider_id;

---

ALTER TABLE atlas_app.person DROP COLUMN organization_id;
-- DROP TABLE atlas_app.organization;
-- We'll delete in the next PR
