library(rgee)


terrainCorrection = function (image) {

  imgGeom = image$geometry()
  # imgGeom$getInfo()
  SRTM <- ee$ImageCollection("JAXA/ALOS/AW3D30/V3_2")$select('DSM')$filterBounds(imgGeom);
  ## srtm == Digital elevation model map (heigh above sea level)
  prj =   SRTM$first()$select(0)$projection();
  srtm = SRTM$mosaic()$clip(imgGeom)$setDefaultProjection(prj) ## 30m srtm
  # srtm$getInfo()
  # SRTM$size()$getInfo()
  sigma0Pow = ee$Image$constant(10)$pow(image$divide(10.0))

  theta_i = image$select('angle');
  phi_i = ee$Terrain$aspect(theta_i)$reduceRegion(ee$Reducer$mean(), theta_i$get('system:footprint'), 1000)  $get('aspect');

  ## 2.1.2 Terrain geometry
  alpha_s = ee$Terrain$slope(srtm)$select('slope');
  phi_s = ee$Terrain$aspect(srtm)$select('aspect');

  # alpha_s$getInfo()
  ## 2.1.3 Model geometry
  ## reduce to 3 angle
  phi_r = ee$Image$constant(phi_i)$subtract(phi_s);

  ## convert all to radians
  phi_rRad = phi_r$multiply( pi / 180);
  alpha_sRad = alpha_s$multiply( pi / 180);
  theta_iRad = theta_i$multiply(pi / 180);
  ninetyRad = ee$Image$constant(90)$multiply(pi / 180);

  ## slope steepness in range (eq. 2)
  alpha_r = (alpha_sRad$tan()$multiply(phi_rRad$cos()))$atan();

  ## slope steepness in azimuth (eq 3)
  alpha_az = (alpha_sRad$tan()$multiply(phi_rRad$sin()))$atan();

  ## local incidence angle (eq$ 4)
  theta_lia = (alpha_az$cos()$multiply((theta_iRad$subtract(alpha_r))$cos()))$acos();
  theta_liaDeg = theta_lia$multiply(180 / pi);
  ## 2$2
  ## Gamma_nought_flat
  gamma0 = sigma0Pow$divide(theta_iRad$cos());
  # gamma0dB = ee$Image$constant(10)$multiply(gamma0$log10());
  # ratio_1 = gamma0dB$select('VV')$subtract(gamma0dB$select('VH'));

  ## Volumetric Model
  nominator = (ninetyRad$subtract(theta_iRad)$add(alpha_r))$tan();
  denominator = (ninetyRad$subtract(theta_iRad))$tan();
  volModel = (nominator$divide(denominator))$abs();

  ## apply model
  gamma0_Volume = gamma0$divide(volModel);
  gamma0_VolumeDB = ee$Image$constant(10)$multiply(gamma0_Volume$log10());



  ## we add a layover/shadow mask to the original implmentation
  ## layover, where slope > radar viewing angle
  alpha_rDeg = alpha_r$multiply(180 / pi);
  layover = alpha_rDeg$gt(theta_i);

  ## shadow where LIA > 90 - 0=ok, 2=shadow
  shadow = theta_liaDeg$gt(85)$multiply(2)

  ## FP change to 1 to 3, 1=ok, no layover or shadow, 2=layover, 3=shadow  4=both?
  ##
  shadowLayover = layover$add(shadow)$add(1)$rename(c("shadowLayover"))$toByte();
  ## calculate the ratio for RGB vis
  # ratio = gamma0_VolumeDB$select('VV')$subtract(gamma0_VolumeDB$select('VH'))

  output = gamma0_VolumeDB
  ## this trick avoids errors from non-matching overlaps between Sentinel-1 footprints and
  ## ALOS-2 DSM
  ee$Image(ee$Algorithms$If(
    SRTM$size(),
     # output$addBands(c(theta_liaDeg, shadowLayover))$rename( c('VV', 'VH', 'angle', 'LIA', 'shadowlayovermask') ), #$select( c('VV', 'VH')  ) ,
    output$select(c(0,1) )$updateMask( shadowLayover$eq(1) ),
    ee$Image(0)$selfMask()$set('isNull', TRUE)$addBands(  ee$Image(0)$selfMask()$set('isNull', TRUE) )$rename( c('VV', 'VH')  )$toFloat()
  ))


  # output$addBands(theta_liaDeg)$toFloat()$addBands(shadowLayover)$toFloat()$addBands(alpha_rDeg)$toFloat()$addBands(theta_i)$toFloat()$rename( c('VV', 'VH', 'angleStrange', 'lia', 'shadowLayover', 'alpha_rDeg', 'angle') )$select( c('VV', 'VH', 'lia', 'angle')  ) ;

}
