--
-- TOC entry 3000 (class 0 OID 16410)
-- Dependencies: 206
-- Data for Name: organization; Type: TABLE DATA; Schema: atlas_gateway; Owner: atlas
--

INSERT INTO atlas_gateway.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, head_count, created_at, updated_at, callback_api_key, info) VALUES
    ('test-provider-2                     ', 'Test provider 2', 'JUSPAY.BPP.MOCK.1', NULL, 'APPROVED', 'PROVIDER', 'FINAL_MILE_DELIVERY', true, true, NULL, NULL, NULL, NULL, NULL, NULL, 'test-provider-2-key', 'http://localhost:8017/v1', NULL, now(), now(), 'test-provider-2-key', NULL);
INSERT INTO atlas_gateway.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, head_count, created_at, updated_at, callback_api_key, info) VALUES
    ('provider-wrapper                    ', 'Fmd wrapper', 'fmd-wrapper', NULL, 'APPROVED', 'PROVIDER', 'FINAL_MILE_DELIVERY', true, true, NULL, NULL, NULL, NULL, NULL, NULL, 'fmd-wrapper-key0', 'http://localhost:8018/v1', NULL, now(), now(), 'test-bpp-key0', NULL);
INSERT INTO atlas_gateway.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, head_count, created_at, updated_at, callback_api_key, info) VALUES
    ('test-app-2                          ', 'Test App 2', 'JUSPAY.BAP.MOCK.1', NULL, 'APPROVED', 'APP', 'FINAL_MILE_DELIVERY', true, true, NULL, NULL, NULL, NULL, NULL, NULL, 'test-app-2-key', 'http://localhost:8016/v1', NULL, now(), now(), 'test-app-2-key', NULL);
INSERT INTO atlas_gateway.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, head_count, created_at, updated_at, callback_api_key, info) VALUES
    ('fmd-test-app                        ', 'FMD Test App', 'fmd-test-app', NULL, 'APPROVED', 'APP', 'FINAL_MILE_DELIVERY', true, true, NULL, NULL, NULL, NULL, NULL, NULL, 'fmd-test-app-key', 'http://localhost:8019/v1', NULL, now(), now(), 'gateway-key', NULL);
INSERT INTO atlas_gateway.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, head_count, created_at, updated_at, callback_api_key, info) VALUES
    ('mobility-app                        ', 'Mobility app', 'JUSPAY.MOBILITY.APP.UAT.1', NULL, 'APPROVED', 'APP', 'MOBILITY', true, true, NULL, NULL, NULL, NULL, NULL, NULL, 'mobility-app-key', 'http://localhost:8013/v1', NULL, now(), now(), 'mobility-gateway-key', NULL);
INSERT INTO atlas_gateway.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, head_count, created_at, updated_at, callback_api_key, info) VALUES
    ('a30193df-4f7c-440f-bada-4d46c396d7d0', '[G] Transporter #1', 'JUSPAY.MOBILITY.PROVIDER.UAT.1', NULL, 'APPROVED', 'PROVIDER', 'MOBILITY', true, true, NULL, NULL, NULL, NULL, NULL, NULL, 'mobility-provider-key', 'http://localhost:8014/v1/7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', NULL, now(), now(), 'juspay-gateway-bpp-key', NULL);
INSERT INTO atlas_gateway.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, head_count, created_at, updated_at, callback_api_key, info) VALUES
    ('83dde90a-81d2-404b-ada5-20aac58005e6', '[G] Transporter #2', 'another-test-cabs', NULL, 'APPROVED', 'PROVIDER', 'MOBILITY', true, true, NULL, NULL, NULL, NULL, NULL, NULL, 'mobility-provider-key-2', 'http://localhost:8014/v1/e1f37274-f0aa-4bb3-93a0-2476349487b7', NULL, now(), now(), 'juspay-gateway-bpp-key', NULL);

INSERT INTO atlas_gateway.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, head_count, created_at, updated_at, callback_api_key, info) VALUES
    ('metro-0a-81d2-404b-ada5-20aac58005e6', 'Metro BPP', 'metro-bpp', NULL, 'APPROVED', 'PROVIDER', 'MOBILITY', true, true, NULL, NULL, NULL, NULL, NULL, NULL, 'key1', 'http://4c75-49-207-207-115.ngrok.io', NULL, now(), now(), 'key1', NULL);

INSERT INTO atlas_gateway.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, head_count, created_at, updated_at, callback_api_key, info) VALUES
    ('mobility-app-metro                  ', 'Mobility app', 'JUSPAY.MOBILITY.APP.UAT.2', NULL, 'APPROVED', 'APP', 'METRO', true, true, NULL, NULL, NULL, NULL, NULL, NULL, 'mobility-app-key', 'http://localhost:8013/metro/v1', NULL, now(), now(), 'mobility-gateway-key', NULL);
