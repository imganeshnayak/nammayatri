ALTER TABLE atlas_app.merchant ADD column dir_cache_slot JSON;
UPDATE atlas_app.merchant SET dir_cache_slot = '"[{\"endTime\":\"11:00:00\",\"slot\":1,\"startTime\":\"08:00:00\"},{\"endTime\":\"17:00:00\",\"slot\":2,\"startTime\":\"11:00:00\"},{\"endTime\":\"20:00:00\",\"slot\":3,\"startTime\":\"17:00:00\"},{\"endTime\":\"23:59:59\",\"slot\":4,\"startTime\":\"20:00:00\"},{\"endTime\":\"08:00:00\",\"slot\":4,\"startTime\":\"00:00:00\"}]"';