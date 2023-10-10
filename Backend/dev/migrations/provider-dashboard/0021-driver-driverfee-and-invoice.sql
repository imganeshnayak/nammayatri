insert into atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, created_at, updated_at, user_action_type) (select atlas_bpp_dashboard.uuid_generate_v4() as id,id as role_id,'DRIVERS' as api_entity, 'USER_NO_ACCESS' as user_access_type, now() as created_at, now() as updated_at, 'DRIVER_SUBSCRIPTION_DRIVER_FEE_AND_INVOICE_UPDATE' as user_action_type from atlas_bpp_dashboard.role) on conflict do nothing;