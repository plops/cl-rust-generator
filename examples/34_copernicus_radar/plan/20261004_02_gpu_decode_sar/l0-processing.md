
Skip to main content
Go to homepage

    SentiWiki

Main

    SentiWiki
    Copernicus Programme
    Copernicus Applications
    Satellite Missions
        Sentinel-1
            S1 Mission
            S1 Applications
            S1 Products
            S1 Processing
        Sentinel-2
        Sentinel-3
        Sentinel-5P
        Future Missions
    Copernicus Operations
    General Information
    Document Library
    Ask Copernicus - AI data guide
    Terms and Conditions

Breadcrumb

    Satellite Missions Sentinel-1 

Use with AI
On this Page

    Processing Baseline
        L0 Processing Baseline
        L1 - L2 Processing Baseline
            SETAP Processing Baseline
    Product Slicing Handling
    L0 Algorithms
        Packet Encoding
        Generation Concept
    L1 Algorithms
        Single Look Complex (SLC)
        Ground Range Detected (GRD)
        Algorithm Overview
        Pre-Processing Algorithms
        SLC Processing Algorithms
        Post Processing Algorithms
        Radiometric Calibration
        ETAD processing
    L2 Algorithms
        Ocean Swell Spectra Processing
        Ocean Wind Field Processing
            Calibration and Pre-processing
            Inversion
        Radial Velocity Processing
    POD Sentinel-1 Products Specification
        Products Description
            Satellite Parameters for POD
            Metrics
            Orbit Comparisons
                AUX_PREORB (first orbit)
                AUX_PREORB (second orbit)
                AUX_RESORB
                AUX_MOEORB
                AUX_POEORB

S1 Processing

This page deepens the information regarding the Sentinel-1 production’s logic and algorithms and it’s targeted to all expert users looking for more details and explanation on product generation. As complementary information to the processing chain and useful for the user who needs to perform interferometry, the final paragraph hosts the Sentinel-1 Precise Orbit Determination products' specifications needed for Sentinel-1 data processing.
Processing Baseline
L0 Processing Baseline

The Sentinel-1 L0 (L0ASP and AIOP) processors history is referred to the packages delivered to reference system.

For the full list of processors' releases, please refer to the SentiBoard Processors page.
L1 - L2 Processing Baseline

The L1-L2 processing baseline is composed by the version of the processors, i.e., of L1 and L2 Instrument Processing Facilities (IPF)s, and by the version of the internal Auxiliary Data Files (ADF)s. Internal Auxiliary Data Files contain information on, e.g., processing parameters, instrument characteristics and system calibration. They might depend on the instrument configuration of an specific unit (ICID), which can change in time, resulting in various existing ADFs and hundreds of processing baselines.

The Internal ADFs are described in the Auxiliary Data File Specification Document (see Product Specification Documents):

    L1

        AUX_PP1: Main L1 processing parameters

        AUX_CAL: Calibration Parameters

        AUX_INS: Instrument Parameters (not available to the public)

    L2

        AUX_PP2: Main L2 processing parameters

        AUX_SCS: Simulated cross-spectra

        AUX_ML2: Machine Learning Models fo L2

Recent ADFs are also available from the Copernicus Data Space Ecosystem (CDSE), and the full ADF archive will be progressively added to the CDSE.

The history of the SAR L1-L2 IPFs since the beginning of S-1 operations is available on the SAR MPC website at the link below:

https://sar-mpc.eu/processor/ipf/

Note that processing results can also be impacted by changes in external resources either from external Auxiliary Data files (AUX_WND, AUX_WAV, AUX_ICE and POD orbits) or internal resources, e.g., DEM used for processing. A detailed description of resources used for processing can be found in the processor ATBDs (see Algorithm Technical Baseline Documentation) and Auxiliary Data File Specification Document (see Product Specification Documents).
SETAP Processing Baseline

The Sentinel-1 ETAD Processor (SETAP) is responsible for the generation of ETAD products. The SETAP processing baseline is composed by the version of SETAP IPF and the version of internal Auxiliary Data Files (ADF)s. Internal Auxiliary Data Files contain information on processing parameters (https://sar-mpc.eu/adf/aux_scf/ ) and timing calibration constants (https://sar-mpc.eu/adf/aux_itc/). Unlike L1/L2 ADFs, SETAP auxiliary files don’t depend on instrument configuration ID.

The history of the SETAP processor since the beginning of its operations in 2023 is available on the SAR MPC website at the link below:

https://sar-mpc.eu/processor/setap/

For the full list of processors' releases, please refer to the SentiBoard Processors page.
Product Slicing Handling

Considering that the SAR instrument can operate up to 25 minutes per orbit in any of the three high-bit rate modes (SM, IW, EW), the Sentinel-1 products are segmented into slices of defined length along a track, to make data more manageable for users. 

Further, feeding sliced Level-0 data to the processing facility enables the ground segment to process slice data in parallel, resulting in more efficient use of resources and more timely product delivery and allowing to archive acceptable throughput.

The Instrument Processing Facility (IPF) is able to generate a Level-1 product that covers the entire Level-0 segment or it can divide the Level-0 segment, in azimuth, into multiple slices.

Level-1 products may be generated by the Sentinel-1 IPF using one of the following processing options:

    Processing an L0 segment of data and generating a single L1 product that covers the segment.

    Dividing the L0 segment of data to process into multiple slices (where slices are overlapping subsets, in the azimuth direction, of the full segment) and processing each slice separately. These slices are processed such that the multiple L1 slice products generated can be recombined into a single, continuous L1 product after all the slices have been processed. The rationale for the slicing scenario is to enable the processing of a segment of data in parallel to increase processor throughput for SM, IW and EW acquisition modes.

image-20231026-142735.png
Figure 1: Data splitting for slicing scenario

From each Level-0 segment, all Level-1 slices are generated using the same set of processing parameters. The image data is continuous in terms of geometry, radiometry and phase, and the annotations are coherent in terms of update rate and grid spacing.

The amount of black-fill at near and far range of the Level-1 image can vary with the segment length. The longer the segment the more black-fill is potentially needed.

Slice products can be combined to form an assembled Level-1 product with the same product characteristics covering the complete segment. Assembly is performed following the three strategies of Include, Merge and Concatenate.

    Include - the value of the information is identical for all slices and a single occurrence of the value is copied into the assembled product.

    Merge - the value of the information may differ between slices and a single value must be amalgamated into the assembled product using the values from all slices. This can be accomplished by means of averaging, majority polling, summing, etc.

    Concatenate - the information is stored in list format and the values from each slice are appended to the appropriate list in the assembled product in Zero Doppler Time (ZDT) ordered sequence and the list count attribute is updated to contain the number of items in the concatenated list. This applies to both binary image data and XML lists.

Level-2 products may be generated from a Level-1 product covering a segment of data (non slicing scenario). In this case, the products correspond to the segment and are called Level-2 individual scene products.

Level-2 products may also be generated from an L1 slice product that covers a single slice (slicing scenario before concatenation of L1 slices was performed). In this case, Level-2 products correspond to a single slice and are called Level-2 slice products.

Fields in the annotations are included, merged and concatenated in accordance with the Sentinel-1 Product Specification document and Sentinel-1 Product Definition.
L0 Algorithms

Level-0 SAR products contain the raw SAR data from the unprocessed Instrument Source Packets (ISP).
Level-0 products are sub-divided into four product classes:

    SAR Level-0 standard products

    SAR Level-0 calibration products

    SAR Level-0 noise products

    SAR Level-0 annotation products.

Standard Level-0 products represent the stream of ISPs containing SAR echo, calibration or noise signal. Level-0 calibration products represent the calibration pulses as extracted from the SAR ISPs stream. Level-0 noise products represent the noise and noise-equivalent (or travelling) pulses as extracted from the SAR ISPs stream. Level-0 annotations products contain the primary and secondary headers extracted from the SAR ISPs stream.

Raw data products are generated from the Sentinel-1 image modes:

    Stripmap Mode (SM)

    Interferometric Wide Swath Mode (IW)

    Extra Wide Swath mode (EW)

    Wave Mode (WV).

Raw data products are also generated from the Sentinel-1 calibration modes:

    RF Characterization Mode (RFC)

    Elevation Notch Acquisition Mode (EN)

    Azimuth Notch Acquisition Mode (AN).

Up to Autumn 2023, only L0 standard products from the SM, IW, EW, WV modes have been distributed by the Ground Segment. From that moment on, corresponding Noise, Calibration and Annotation products are available to the expert users too on Copernicus Data Space Ecosystem: for these products the retention is of two weeks.

Level-0 products may include data in single polarisation mode (HH or VV) or, for SM, IW, and EW modes, they may include dual polarisation modes (HH+HV or VV+VH). For dual polarisation data, data for each polarisation are available in separate files within the Level-0 product.
Packet Encoding

Raw SAR data compression is used to reduce the instrument data rate to fit the available data downlink. Application of a Block Adaptive Quantisation-like (BAQ) raw data compressor adds quantisation noise to the raw data which degrades the data Signal-to-Noise Ratio (SNR). A BAQ that applies a constant bit rate independent of radar signal power is not optimal. It produces a degradation of the SNR which varies with the power of the detected radar signal. To overcome this, Sentinel-1 Level-0 data products are compressed using Flexible Dynamic Block Adaptive Quantisation (FDBAQ).

FDBAQ controls the bit rate in a flexible way as a function of the input radar signal power to provide a variable bit rate coding that increases the number of bits to be allocated to bright scatterers. The number of quantisation bits is selected according to a local estimate of clutter-to-noise ratio. This approach leads to a time variant bit rate.

FDBAQ is an improvement over the Entropy Constrained Block Adaptive Quantisation (ECBAQ) used for ENVISAT.

For noise and calibration data BAQ 5 bits compression algorithm is used instead.

For the data to be usable, it will need to be decompressed following the decompression algorithm detailed in the SAR Space Packet Protocol Data Unit.
Generation Concept

Due to the possibility of fragmentation of payload data downlinks over different Receiving Stations or over different passes, the SAR Level 0 data generation concept includes the need of a Level 0 consolidation functionality which is aimed at assembling “partial” (i.e. covering only part of an acquisition segment) Level 0 segments.

The processing chain is composed by two different steps:

    AIO Processor (AIOP): the All-In-One (AIO) Processor is able to process Channel Access Data Unit (CADU) streams and generate the corresponding L0 SAR, GPS, AIS (S1C/D only) and HKTM files. It extracts the Instrument Source Packets (ISP) from the CADUs, generating ISP annotations, it applies a Reed-Solomon correction and it performs SAR Packets processing for Annotation, Calibration, Noise and Segment L0 Products generation. This step is mandatory.

    L0 Assembly/Slicing Processor (L0ASP): the L0 Assembly/Slicing Processor assembles partial segments of Level 0 Data Takes acquired at different Stations to generate a Complete Level 0 Product and it is moreover responsible to assemble the horizontal and vertical parts of a dual polarisation product. The assembled Level 0 Product is then cut into Level 0 Slices of approximately 25 seconds, used to feed the higher level productions.

The orchestration of the two processors (when one or both of the steps are called) is out of the scope of this generation chain description: as a matter of fact, the majority of L0 products come out of the L0ASP. The result of the L0 generation chain are SAR L0 products covering complete data take, in the form of slices and the Annotation, Calibration and Noise products.
L1 Algorithms

The processing steps involved to produce Level-1 data products include pre-processing, Doppler centroid estimation, single look complex focusing, and image and post-processing for generation of the SLC and GRD products as well as mode specific processing for assembling of multiple sub-swath products.
image-20231026-143022.png
Figure 2: Level-1 Processing Flow

For converting digital pixel values to radiometrically calibrated backscatter, all the required information can be found in the product. A calibration vector is included as an annotation in the product allowing simple conversion of image intensity values into sigma or gamma nought values. Further details of how to calibrate Sentinel-1 Level 1 products can be found in the 'Radiometric Calibration of S-1 Level-1 Products Generated by the S-1 IPF' document.
Single Look Complex (SLC)

Level-1 Single Look Complex (SLC) products consist of focused SAR data, geo-referenced using orbit and attitude data from the satellite, and provided in slant-range geometry. Slant range is the natural radar range observation coordinate, defined as the line-of-sight from the radar to each reflecting object. The products are in zero-Doppler orientation where each row of pixels represents points along a line perpendicular to the sub-satellite track.

The products include a single look in each dimension using the full available signal bandwidth and complex samples (real and imaginary) preserving the phase information. The products have been geo-referenced using the orbit and attitude data from the satellite and have been corrected for azimuth bi-static delay, elevation antenna pattern and range spreading loss. Fine timing corrections are available as separate auxiliary product for advanced users see ETAD products.

SM SLCs contain one image per polarisation for its single swath. IW, having three sub-swaths, has three images in single polarisation and six images for dual polarisation. EW, having five sub-swaths, has five images for single polarisation and ten images for dual polarisation products.

For IW and EW, each sub-swath consists of a series of bursts in azimuth. The individually focused complex burst data are included, in azimuth-time order, into a single sub-swath image, with black-fill demarcation in between.

For IW, a focused burst has a duration of ~2.75 seconds and a burst overlap of approximately ~0.4 seconds. For EW, a focused burst has a duration of ~3.19 seconds with an overlap of ~0.1 seconds. The overlap slightly increases in range within a sub-swath. This overlap is sufficient to provide contiguous coverage of the ground.

Images for all bursts in all sub-swaths of IW/EW SLC products are re-sampled to a common pixel spacing grid in range and azimuth. Burst synchronisation is maintained for both IW and EW products to ensure that interferometry between pairs of products acquired multiple repeat periods apart can be performed.

The Swath Timing data set record in SLC products contains information about the bursts including dimensions, timing and location that can be used to merge the bursts and swaths together. Sentinel-1 performs systematic acquisition of bursts in both IW and EW modes. The bursts overlap almost perfectly between different passes and are always located at the same place. With the deployment of the SAR processor S1-IPF 3.4, a new element has been added to the products annotations: the Burst ID, which should help the end user to identify a burst area of interest and facilitate searches. The Maps linking the Burst ID to the corresponding geographic footprint are publicly available at https://sar-mpc.eu/test-data-sets.
Ground Range Detected (GRD)

Level-1 Ground Range Detected (GRD) products consist of focused SAR data that has been detected, multi-looked and projected to ground range using the Earth ellipsoid model WGS84. The ellipsoid projection of the GRD products is corrected using the terrain height specified in the product general annotation. The terrain height used varies in azimuth and it is constant in range (For IW/EW modes only the terrain height of first subswath is considered)

Ground range coordinates are the slant range coordinates projected onto the ellipsoid of the Earth. Pixel values represent detected amplitude. Phase information is lost. The resulting product has approximately square resolution pixels and square pixel spacing with reduced speckle at a cost of reduced spatial resolution.

For the IW and EW GRD products, multi-looking is performed on each burst individually. All bursts in all sub-swaths are then seamlessly merged to form a single, contiguous, ground range, detected image per polarisation.
Algorithm Overview

The ESA Earth Observation Framework - Copernicus Space Component (EOF-CSC) is responsible for:

    implementing the Sentinel-1 mission observation scenario

    X-band data reception

    generating the operational core ground segment products

    reacting to emergency orders with rush instrument tasking and processing

    providing access to Sentinel-1 data

    ensuring long term mission data archiving

    monitoring the instrument and mission performance

    ensuring Sentinel-1 core operational user products meet the expected quality, with necessary calibration and validation activities.

Within the EOF-CSC, the Instrument Processing Facility (IPF) is responsible for the generation of Level-1 and Level-2 products. Level-0 data are ingested into the system where different pre-processing steps are performed. The system then applies the Single Look Complex (SLC) image processing for Doppler centroid estimation and focusing, as well as mode-specific processing for assembling of multi-swath products. The Level-1 post-processing then generates the output Level-1 SLC products and the Level-1 Ground Range Detected (GRD) products. From internally generated Level-1 SLC and GRD products, the Level-2 Ocean (OCN) products are produced. 

To accomplish this, the IPF is composed of several software components as shown in the following figure.
image-20231026-143319.png
Figure 3: Sentinel-1 IPF Architecture

The algorithms applied by the IPF to generate Level-1 products from Level-0 data include:

    Level-1 pre-processing

    Special handling for TOPSAR mode

    Doppler centroid estimation

    Level-1 SLC processing

    Level-1 post-processing

    Level-1 Radiometric Calibration.

Processing of each polarisation is essentially the same. A separate image is generated for each polarisation, and polarisation-specific correction and calibration factors are applied during the processing of each polarisation The Doppler Centroid (DC) from the co-polarisation channel is used for processing both polarisations, so that the two resulting images are accurately co-registered.

For further details on the Sentinel-1 Level-1 algorithms, please refer to the Sentinel-1 Level-1 Detailed Algorithm Definition document.

TOPSAR Processing

The Terrain Observation with Progressive Scans SAR (TOPSAR) technique is a form of ScanSAR imaging, where data is acquired in bursts by cyclically switching the antenna beam between multiple adjacent sub-swaths. TOPSAR acquisitions can provide large swath widths and enhanced radiometric performance by reducing the scalloping effect. TOPSAR is used in Sentinel-1's Interferometric Wide swath and Extra Wide swath modes.

With the TOPSAR technique, in addition to steering the beam in range as in ScanSAR, the beam is also electronically steered from backward to forward in the azimuth direction for each burst, avoiding scalloping and resulting in homogeneous image quality throughout the swath. [1]
Schematic representation of the TOPSAR Mode Acquisition .png
Figure 4: Schematic representation of the TOPSAR Mode Acquisition

TOPSAR mode is intended to replace the conventional ScanSAR mode, achieving the same coverage and resolution as ScanSAR, but with a nearly uniform Signal-to-Noise Ratio (SNR) and Distributed Target Ambiguity Ratio (DTAR).

Azimuth resolution is reduced compared to SM due to the shorter target illumination time of the burst. Using the sweeping azimuth pattern, each target is seen under the same antenna pattern, independently from its azimuth position in the burst image. By shrinking the azimuth antenna pattern, as seen by a target on the ground, scalloping effects on the image can be reduced. Bursts are synchronised from pass to pass to ensure the alignment of interferometric pairs.

For TOPSAR, the processing must handle the antenna steering rate and the DC rate due to the steering. The azimuth pre and post-processing of the data must include de-ramping of the data prior to base-band DC estimation, azimuth ambiguity estimation and GRD azimuth processing. Please see the technical note COPE-GSEG-EOPG-TN-14-0025 for details on how deramping is performed by the IPF.

TOPSAR azimuth antenna sweeping causes Doppler centroid variations of approximately 5.5 kHz introducing an azimuth phase ramp (azimuth fringes) for small co-registration errors. To correct this, azimuth co-registration is required to be better than 0.001 samples in order to obtain phase error less than 3°.

To be useful for generating interferograms, TOPSAR bursts are synchronized between repeat-pass data takes. A burst synchronization of <5 ms is required.

 [1] De Zan, F., & Guarnieri, A. M. (2006). TOPSAR: Terrain Observation by Progressive Scans. Geoscience and Remote Sensing, IEEE Transactions on, 44(9), 2352-2360. doi:10.1109/TGRS.2006.873853
Pre-Processing Algorithms

Preprocessing is applied to the Level-0 products before any parameter estimation and image formation steps are performed. Pre-processing includes four main components:

    Raw data analysis.

    Internal calibration.

    Downlink header validation.

    Terrain height function.

Raw Data Analysis

The Sentinel-1 SAR instrument's receive module performs demodulation in the digital domain. Therefore, unlike ENVISAT ASAR which requires corrections to the I and Q channels of the raw signal, for Sentinel-1, I and Q channel gain imbalance and non-orthogonality corrections are not necessary.

Internal Calibration

Internal calibration is based on information extracted from the calibration and noise measurements associated with a data-take as part of the initial, interleaved and final internal calibration sequence performed by the instrument. Internal calibration consists of:

    calibration pulse extraction

    replica reconstruction

    reference replica derivation

    instrument drift derivation

    replica whitening

    replica and PG product validation

    noise measurement processing.

Noise Measurement Processing

Noise measurements are recorded during each data-take as part of the initial and final internal calibration packet. Noise measurements are recorded separately for each beam by switching off the TX signal for a sufficient number of TX pulses. In addition, noise-equivalent measurements are represented by the travelling echoes after each interleaved calibration sequence all along the data-take. The noise level is computed as the mean power of the available noise packets for each beam.
Noise Measurement Processing.png
Equation 1:


In this equation, M is the number of noise packets and N is the number of samples.

Downlink Header Validation

The downlink header validation step is performed to validate the fields of the downlink. It will detect bit errors, missing lines and gaps in the data.

The validation also checks transmission patterns to determine whether the downlink is as expected for such things as the order of swaths, length of bursts and sequence of echo versus calibration and noise packets.

Terrain Height Function

The terrain height function assembles a vector of terrain height values along the azimuth direction of the scene. Each terrain height value is obtained for a particular azimuth time by averaging over a DEM for an azimuth block and covering the entire range dimension of the scene. The azimuth block can be configurable to any number of azimuth lines or for every single azimuth line.

The IPF uses the Global Earth Topography And Sea Surface Elevation (GETASSE30) DEM version 2. The GETASSE30 DEM is a composite of four other data sets: the SRTM30 data, the Altimeter Corrected Elevations (ACE) DEM, the Mean Sea Surface (MSS) data and the EGM96 ellipsoid. The resulting GETASSE30 DEM represents the earth topography and sea surface elevation with respect to the WGS84 ellipsoid at 30 arc second resolution.

In the case of terrain height values being averaged over several azimuth lines, if later processing requires an intermediate value that value is obtained by linear interpolation.

A single terrain height vector is used for different polarisations and for all sub-swaths in IW and EW modes. This Terrain Height is then reused by later processing steps that may consider the values for all subswaths or only for the first one (to avoid introducing artefacts in the generated products).

Doppler Centroid Algorithms

The Doppler Centroid (DC) algorithms estimate the centre frequency of the Doppler spectrum of the data, related to the azimuth beam centre. The DC locates the azimuth signal energy in the azimuth frequency domain. It is needed so that the signal energy in the Doppler spectrum can be correctly captured by the azimuth compression filter, providing the best signal-to-noise ratio and azimuth resolution.

The DC varies in both range and azimuth. The range variation depends on the satellite attitude and how closely the illuminated footprint on the ground follows an iso-Doppler line on the ground as a function of range. The azimuth variation is caused by slow changes in satellite attitude as a function of time.

The DC is estimated at different ranges in the data and a polynomial function of range is fitted to the measurements. The DC is updated in successive azimuth blocks.
image-20231026-142833.png
Figure 5: Doppler Centroid Estimation

The Doppler is composed of a fine DC frequency (the fractional part), which is ambiguous to within the Pulse Repetition Frequency (PRF) and a second absolute component, which is an integer multiple of the azimuth sampling rate.

The DC estimation algorithm includes the following steps:

    Absolute DC calculation from orbit and attitude.

    Fine DC estimation.

    Weighting factors calculation.

    Fine DC estimate unwrapping.

    Absolute DC estimation.

For TOPSAR data, the data must be demodulated in the azimuth direction before applying the phase-based correlation DC estimator.
SLC Processing Algorithms

Single Look Complex (SLC) processing focuses the data in azimuth and range to form an image. The SLC processing takes as input, the signal data and the pre-processing output including orbit information and Doppler centroid estimation polynomials. The processing then applies range processing, azimuth pre-processing, azimuth processing and azimuth post-processing, as shown in the figure below.
image-20231026-143527.png
Figure 6: SLC Processing Algorithm

Each burst in each sub-swath for TOPSAR products is processed independently. The independently focused burst images are then included in a single sub-swath image ordered by azimuth time and separated by black-fill.

Range Processing

Range processing consists of:

    range reference function

    range compression

    range dependent gain correction

    Sampling Window Start Time (SWST) bias correction.

The SWST bias is accounted for by adding it to the range start time.

Azimuth Pre-Processing

Azimuth pre-processing applies azimuth zero-padding, azimuth forward FFT and, for TOPSAR products, azimuth frequency unfolding and resampling.

Azimuth pre-processing converts the data into the range-Doppler domain needed for the range-Doppler algorithm in the azimuth processing.

Azimuth Processing

The azimuth processing utilises the range-Doppler algorithm characterised by a hyperbolic range equation.
Azimuth Processing (SLC) hyperbolic range equation.png
Equation 2:

The range-Doppler algorithm is computationally efficient and, for typical space-borne imaging geometries, the range-Doppler algorithm is an accurate approximation of the exact SAR transfer function. The algorithm is phase-preserving and SLC images formed with it can be used for applications such as interferometry.

Secondary Range Compression

For range compression, raw data is multiplied in the frequency domain by the range reference function. This multiplication is carried out with complex values, i.e. the phase information in the data is preserved.

Range Cell Migration Correction

The range-Doppler algorithm uses the large difference in timescale of range and azimuth data and approximately separates processing in these two directions using Range Cell Migration Correction (RCMC). Target trajectories need to be corrected to account for the fact that the instantaneous slant range changes with azimuth time by moving all responses of a target from the trajectory into a straight line. RCMC is performed in range frequency and azimuth frequency domain.

For TOPSAR data, a range resampling step is combined with RCMC.

Azimuth Compression

Azimuth compression applies an azimuth matched filter to each azimuth line and applies the inverse FFT to the azimuth line. The result is a focused image.

Azimuth post-processing

Azimuth post-processing is for TOPSAR products only, to resample to a common grid. This resolves issues with time aliasing due to the limited support during azimuth focusing. The algorithm includes the following steps:

    Mosaicking.

    De-ramping.

    Low-pass filtering and resampling.

    Re-ramping.

Post Processing Algorithms

Post-processing generates the output SLC and GRD products as well as quicklook images. Post-processing consists of post-processing range processing, post-processing azimuth processing and post-processing output processing as shown in the figure below.

The processing is applied to each sub-swath for IW and EW and for each azimuth block consisting of a burst for IW and EW and an entire vignette for WV.
image-20231026-143213.png
Figure 7: Post-Processing Algorithm

Post-Processing Range Processing

Post-processing range processing is performed on each range line within an individual burst of a sub-swath for TOPSAR or vignette for WV.

Post-Processing Azimuth Processing

Post-processing azimuth processing is performed on each azimuth line for each range look within an individual burst of a sub-swath for TOPSAR or vignette for WV.

Post-processing GRD Output Processing

For GRD products, the post-processing output processing consists of optionally removing the thermal noise, de-burst and merge for TOPSAR mode, generation of quicklooks and writing to the output file format. In Sentinel-1 A/B, every data-take contains two noise acquisitions, one at the beginning of the data-take and a second acquisition at the end. In addition, noise-equivalent acquisitions are represented by the travelling echoes after each interleaved calibration sequence all along the data-take. For Level-1 GRD products only, the thermal noise contribution is estimated and optionally removed to improve the quality of the image.

The thermal noise contribution is reshaped in a range and azimuth varying fashion by the range and azimuth varying radiometric corrections applied by the SAR processor. In particular for multi-swath products, the noise can be different between swaths causing an intensity step at swath boundaries (See Thermal denoising of products generated by the Sentinel-1 IPF).

For a particular azimuth time, thermal noise is estimated in slant range coordinates as follows:

    Calculate the range spreading loss vector.

    Calculate the elevation beam pattern vector.

    Apply the scalar contributing factors.

For TopSAR modes a further contribution is present along the azimuth direction (scalloping effect) and consists in the azimuth elementary beam pattern.

For SLC products, the noise estimation vectors are then only saved in the annotation to allow for later use in removal and as input for Level-2 processing.

For GRD products, the thermal noise vectors are converted to ground range coordinates and applied to the data by subtracting the noise from the power-detected image.

When calibrating the product to β, σ or γ, the noise vector must be scaled by the corresponding calibration Look-Up Table (LUT) (β, σ or γ or dn, respectively):

Equation 3:

where, depending on the LUT selected to calibrate the image data:

noise(i) = calibrated noise profile for one of β0i, σ0 , or yi or originalDNi

ηi = noiseLut (i)

Ai = one of betaNought (i) , simaNough t( i) , gamma(i) , dn (i)


Once the calibrated noise proﬁle has been obtained, the noise can be removed from the GRD data by subtraction.

For any pixel i that falls between points in the LUT the value is found by bilinear interpolation.

During TOPSAR sub-swath merging, the noise vectors are also merged into one vector following the same strategy as for merging two adjacent ground range images.

Within the annotation of the resulting product, the noise vector annotation data set contains the thermal noise estimation along azimuth. This can be used to reverse the thermal noise correction in products in which it has been applied or can be used for applying the correction in products in which it has not.

TOPSAR Debursting and Sub-Swath Merging

The TOPSAR modes acquire data in bursts and for several sub-swaths. For GRD products, the bursts are concatenated and sub-swaths are merged to form one image.

Bursts overlap minimally in azimuth and sub-swaths overlap minimally in range. Bursts for all beams have been resampled to a common grid during azimuth post-processing.

For merging the sub-swaths, the optimal cut is determined taking half the overlap between them, considering only the valid samples of each line. Samples from two consecutive sub-swaths are put side-to-side according to the optimal cut position (without performing sample 'blending').

In the azimuth direction, bursts are merged according to their zero Doppler time. Note that the black-fill demarcation is not distinctly zero at the end or start of the burst. Due to resampling, the data fades into zero and out. The merge time is determined by the average of the last line of the first burst and the first line of the next burst. For each range cell, the merging time is quantised to the nearest output azimuth cell to eliminate any fading to zero data.

Application LUT Scaling

Application LUTs are used to apply a range-dependent gain function to the processed data prior to generation of the final image output. The application LUT scaling is used to optimise the radiometric scaling of the main feature of interest, while optimising the available dynamic range in the output product and to compensate for changes in the radar backscatter with changing incidence angles. LUT's that could be used include:

    Point target application LUTs - suited to applications involving scattering from bright points targets. Typically these LUTs provide poor quantisation over areas of very low backscatter.

    Sea, land, mixed and ice LUTs - suited to the thematic applications they describe in which low backscatter features are expected. Typically, bright targets will saturate. Values vary with incidence angle.

    General application LUT - typically very bright targets will be saturated.

During satellite Commissioning Phase and related calibration activities, a general purpose LUT has been defined for each acquisition mode, allowing to avoid saturation of both brighter and darker targets and at the same time to reduce as much as possible the quantization error.

Level-1 SLC and GRD images are then scaled to 16 bit and saved in GeoTIFF file format.

Quicklook Generation

Quicklooks are lower resolution images of the product data used to preview the data. Quicklooks are generated by power detecting, averaging and decimating in both azimuth and range directions by a configurable amount. IW and EW SLC product quicklooks are first de-burst and merged.

The Red channel is the co-polarisation amplitude, the Green channel is the cross-polarisation amplitude and the Blue channel is the ratio of Green to Red channels. So for a HH/HV dual polarisation product, Red is HH, Green is HV. Separate scaling of each of the RGB channels is used to give a false colour image where the land is green and the ocean is generally blue (or sometimes red under higher wind conditions).

Quicklook images are scaled to 8 bit and saved in PNG file format.
Radiometric Calibration

Application of Radiometric Calibration LUT

The objective of SAR calibration is to provide imagery in which the pixel values can be directly related to the radar backscatter of the scene. To do this, the output scaling applied by the processor must be undone and the desired scaling must be reintroduced. Level-1 products provide four calibration Look Up Tables (LUTs) to produce ß0i, σ0i and γi or to return to the Digital Number (DN). The LUTs apply a range-dependent gain including the absolute calibration constant. For GRD products a constant offset is also applied.

The radiometric calibration is applied by the following equation:

Equation 4:

where, depending on the selected LUT,
image-20230510-154837.png

Bi-linear interpolation should be used for any pixels that fall between points in the LUT.

Application of De-noise LUT

Level-1 products provide a noise LUT for each measurement data set. The values in the de-noise LUT, provided in linear power, can be used to derive calibrated noise profiles matching the calibrated GRD data. The de-noise LUT must be calibrated matching the radiometric calibration LUT applied to the DN:

Equation 5:

where, depending on the LUT selected to calibrate the image data,
image-20230510-154752.png

The calibrated noise profile can then be applied to the data to remove the noise by subtraction. Application of the radiometric calibration LUT and the calibrated de-noise LUT can be applied in one step as follows:

Equation 6:

Bi-linear interpolation should be used for any pixels that fall between points in the LUT.
ETAD processing

Timing corrections for SAR imagery

The raw Sentinel-1 range and azimuth timings as annotated to the Level 1 SLC products require additional corrections to deliver consistent results in repeat pass geolocation and feature analysis at the sub-pixel level. The corrections comprise the delays stemming from the Earth’s atmosphere, the direct tidal deformation of Earth’s crust caused by Sun and Moon, and the Sentinel-1 system corrections associated with the methods of the Sentinel-1 SAR-IPF when annotating the level 1 products. The corrections are summarised and distributed by the Extended Timing Annotation Dataset for Sentinel-1 (S1-ETAD). In the Sentinel-1 ETAD Algorithm Technical Baseline Document methods defining the algorithmic cores of the Sentinel-1 Extend Timing Annotation Processor (SETAP) are deeply described.

Overview on Processing Steps

The SETAP computations are based on the sliced Sentinel-1 L1 SLC products forming a data take, which are provided to the processor along with the precise orbit ephemeris product, see Figure 8 below. Moreover, the different computational algorithms have access to their respective model data. These data are either provided by the external management layer to the processor (ECMWF numerical weather model data, TEC map data) or are part of the processor itself (global DEM, planetary ephemerides data, geoid data).
image-20230912-093952.png
Figure 8: Overview on the SETAP computational algorithms with their corresponding data access

Following the computational flow outlined in the figure, the first step is the preparation of the correction grid covering the extent of the data take. The grid is calculated in range and azimuth as two-way timeof-flight and seconds of day UTC, respectively, according to a predefined spacing. The default spacing is specified to achieve on average a 200 m sampling in both dimensions. This grid of radar timings has to be geolocated on the DEM in order to define the 3D sensor-to-ground geometry of the data take, which involves the algorithms:

    Correction Grid Definition: level 1 image timing annotations are used to create a consistent grid covering the data take which can be decomposed into sub-grids covering the slices

    Satellite State Vector Interpolation: the discretely sampled state vectors (position vector, velocity vector) of the precise orbit product are interpolated for the azimuth times of the computation grid. 

    Correction Grid Geolocation: the 2D radar timings linked to corresponding satellite state vectors are geolocated on the DEM to obtain the 3D coordinates in the global terrestrial frame.

With the satellite positions and the 3D coordinates of the computational grid known, the individual correction algorithms can be independently computed (Figure 8 lower part):

    Direct Integration of NWM for Tropospheric Delays: the 4D numerical weather model data (given at discrete positions and time steps) are interpolated to obtain the refractivity along the sensor to ground line-of-sight, which is then integrated to derive the tropospheric path delay in range.

    Ionospheric Delay Computation from TEC Maps: the delay in range caused by the dispersive ionosphere is derived from the global total electron content (TEC) maps containing the integrated free electrons inferred from global GNSS observations.

    Solid Earth Tidal Displacement Computation: the tidal deformation of the Earth’s crust by Sun and Moon is computed by the conventional geodynamic model associated with the geodetic reference frames and converted into timing corrections of range and azimuth

    Sentinel 1 system correction: The Sentinel-1 system specific corrections deal with the sub-pixel level deviation of the IPF SLC product annotation with respect to conventional zero-Doppler geometry. The corrections allow for the refinement of the azimuth and range annotation in post-processing without modifying the SLC image. Three corrections are required:

        Bistatic Azimuth Effects Mitigation: the corrections for the residual bistatic affects in azimuth stemming from the movement of the satellite during the SAR acquisition are computed using the annotations of the S-1 input products.

        Doppler-induced Range Shift Mitigation: the corrections for the range shifts caused by the focussing of the Doppler-shifted radar pulses in Sentinel-1 TOPS modes are computed based on the annotations of the S-1 input products.

        FM-rate Mismatch Azimuth Shift Mitigation: the corrections for the azimuth shifts due to the mismatch of azimuth FM-rate, which is derived by the Sentinel-1 IPF applying topographic assumptions, are computed using the DEM and the annotations of the S-1 input products.

For further details on the algorithms, refer to Sentinel-1 ETAD Algorithm Technical Baseline Document.
L2 Algorithms

Level-2 OCN products are processed by the Level-2 Instrument Processing Facility (IPF). For each OSW, OWI and RVL component:

    the appropriate Level-1 internal product is read

    the variables corresponding to the considered component are estimated

    a temporary file containing the results is saved locally

The figure below shows the Level-2 processing workflow at the IPF. External IPF interfaces have a white background, internal IPF interfaces are identified by a grey background.
image-20231026-143130.png
Figure 9: Level-2 Processing Workflow

During the processing, a job order is read by the processor retrieving high level information required for processing a particular product (e.g. names and directories of input Level-1 files, names and directories of auxiliary data files, directories of outputs files, etc). Processing then starts from Level-1 products using the auxiliary data files provided such as the Level-2 processor parameter file. During the processing, a log file is generated to monitor the status of each processing step. The final step of the processing is the creation of the product including writing of all the geophysical information into netCDF files.

The SM and TOPS modes have the dual-polarisation option. However, the Level-2 OCN components are always estimated only using the information from the co-polarised signal. As a result, the algorithms for each component, as well as the workflow for the Level-2 OCN product generation, are not different from that of single polarisation product.

Separate processing of the components include:

    Level-2 Ocean Swell Spectra (OSW) processing

    Level-2 Ocean Wind Field (OWI) processing

    Level-2 Radial Surface Velocity (RVL) processing

Ocean Swell Spectra Processing

The OSW products are two-dimensional ocean surface wave spectra estimated from a Level-1 SLC by inversion of the corresponding image cross-spectra. The cross-spectra are computed by performing multi-looking in azimuth followed by co- and cross-spectra estimation among the detected individual look images. A single OSW can be computed from an Wave mode SLC imagette or from a sub-image extracted from a Stripmap mode SLC image.

The OSW product cannot be generated from the TOPSAR mode, since individual looks with sufficient time separation are required. The obtained inter-look time separation within one burst is too short due to the progressive scanning (i.e. short dwell time). Individual looks from neighbouring bursts require significant spatial overlap.

The Sentinel-1 wave processing system consists of:

    a spectral estimation unit

    a spectral inversion unit

image-20231026-142926.png
Figure 10: High Level OSW Processing Algorithm

The OSW processing will use the following inputs:

    SM or WV SLC product

    External auxiliary data:

        ECMWF atmospheric model data

        Level-2 processor parameters auxiliary data

        Simulated cross-spectra data

    Internal auxiliary data:

        coastline and land masking data

        General Bathymetry Chart of the Oceans (GEBCO)(in case of SM data)

        Range Fourier profile

        IPF Level-2 internal parameter file containing extra processing parameters specific to the OSW algorithm.

Spectral Estimation

The spectral estimation unit performs the processing from Level-1 SLC product to an internal co- and cross-variance function (Level-1B) product.

The spectral estimation consists of inter-look cross spectral processing based on splitting the azimuth bandwidth into three non-overlapping looks, followed by an estimation of the co- and cross-variance function based on the periodogram method.

The final result consists of one co-variance function and two cross-variance functions on cartesian grid. The two cross-variance functions correspond to the neighbour looks and the outer looks i.e. with two different look separation times. The co-variance function is the average of the co-variance functions from the three individual looks. The processing also estimates the percentage of land within the selected estimation area (in case of SM data), the range and azimuth cut-off wavelength, spectral resolution, and some image statistics (mean, variance, skewness).

The figure below shows the flowchart for the spectral estimation.
image-20231026-143616.png
Figure 11: Spectral estimation unit

Spectral Inversion

The spectral inversion unit generates the Level-2 OSW product using the intermediate Level-1 data product as input.

The OSW spectral inversion unit first accesses the intermediate product and performs a 2D Fourier transform to achieve the co- and cross-spectra on cartesian grid. A Hanning window is used in the 2D Fourier transform. The OSW processing then performs a wave spectral inversion of the co- and cross-spectra with respect to the detected SAR ocean wave-like pattern. This is done by first estimating and removing the non-linear contribution to the imaging process, assuming that this is caused only by the local wind field, and then applying a quasi-linear inversion in the most energetic part of the SAR co- and cross-spectrum. The wind field is therefore required, and this is estimated as described in the OSW processing.

The estimation of wind sea significant wave height is performed using the estimated wind speed and the azimuth cut-off wavelength.

The major requirements for the quality of the inversion is knowledge of the Real Aperture Radar (RAR) Modulation Transfer Function (MTF), the azimuth cut-off wavelength, and an accurate removal of the non-linear part of the spectra (i.e. the wind field). The RAR MTF is computed using a backscattering model including non-uniform distribution of scatterers on the long wave field. After the inversion, the ocean wave spectrum is converted to polar grid, rotated relative to north, partitioned, and ambiguity resolved followed by computation of key spectral parameters for the two most energetic partitions. Finally, an output product is generated from the polar spectra and stored in a netCDF format together with extracted parameters stored as attributes and some key parameters from the corresponding Level-1 product.
Ocean Wind Field Processing

The Sentinel-1 Level-2 OWI component is an ocean surface wind vector (speed and direction) estimated from a Sentinel-1 Level-1 SAR image by inversion of its associated Normalised Radar Cross Section (NRCS).

Sentinel-1 wind processing consists of a calibration unit followed by an inversion unit.
image-20231026-142512.png
Figure 12: High Level OWI Processing Algorithm


The Level-2 OWI processing system will access:

    Level-1 internal GRD product.

    Auxiliary information (through the auxiliary files) such as coastline data (in case of SM, IW and EW).

    Atmospheric model wind speed and direction.

    Ice mask.

    Level-2 processor parameters.

    Environment parameters like input/output file names and paths to read the appropriate auxiliary files are indicated in the job order.

    The processor set-up parameter file PRM_LOP which may contain extra processing parameters specific to the OWI algorithm.

    The processor configuration file used to extract the product report/product list extension and the PRM_LOPIn path.

Calibration and Pre-processing

The calibration unit performs the processing from Level-1 product to a calibrated product containing the parameters required for the wind inversion unit. They are calculated at the resolution of the desired wind cell for each wind cell of the OWI grid (which could be specified in the Level-2 processor parameters file AUX_PP2, normally set to 1km).

Given the size in metres, of the SAR derived wind cells respect to the pixel size in metres (fromLevel-1 input data), and with the parameters for the bright target detection, the number of pixels to be averaged in range (x) and azimuth (y) direction are computed. Based on this, Arrays corresponding to parameters, useful for the OWI processing, such as the intensity image, NRCS, NESZ, longitude and latitude, incidence angle, track angle, two-way slant range time and the percentage of pixels in SAR image detected as bright targets are generated from the L1 internal product.

The a priori wind and ice mask information from external ancillary data (S1_AUX_WND/S1_AUX_ICE) are interpolated on the SAR OWI grid. The coastline and land mask information from internal ancillary data are interpolated on the SAR OWI grid. Those are provided as owiMask variable. In addition, an estimation of calibration constant based on ancillary geophysical parameters is calculated for each SAR image product.
Inversion

The inversion unit generates the Level-2 OWI component using parameters obtained from the calibration unit. The inversion estimates the wind vector using NRCS values, the incidence angles, the track angles and the a priori wind and ice information. This inversion is performed using statistical Bayesian inference. For HH polarisation, the NRCS in HH is transformed to a VV equivalent NRCS. In the case of dual polarisation data, the cross-polarisation NRCS is not used in the wind inversion scheme. However, the cross polarization information (NRCS and annotated NESZ) at the owi grid resolution will be present in the products generated with IPF 2.90 and latter. The possible wind speed values at each wind cell are estimated given the NRCS value and all possible wind directions. At each wind cell, the probability density function of a priori wind vector is computed for all the possible solution couples. Next, the most plausible wind speed and direction values at each SAR wind cell of the SAR wind grid are selected. Lastly, the SAR and ancillary model information consistency is estimated. The OWI component is then saved to be joined with the other components into a netCDF file.
Radial Velocity Processing

The Sentinel-1 Level-2 RVL component consists of an estimate of the total Doppler frequency (Hz) and the corresponding radial velocity (m/s) estimated from an internal Sentinel-1 Level-1 Single-Look Complex (SLC) SAR image. The RVL component contains an estimate of the width of the ocean Doppler spectra. The Doppler width is a new geophysical parameter that has never been estimated from SAR data before. For each of these parameters the RVL component contains the corresponding standard deviation of the estimates. The image from which a single RVL is computed can be a SLC vignette from the WV mode, or sub-image extracted from a SM SLC image, IW SLC image or an EW SLC image. The RVL product is given on a grid similar to the OSW or OWI components.

The Doppler frequency is estimated from the SLC data by fitting (least square minimisation) the antenna model to the observed azimuth spectra taking into account effects from additive noise and side-band effects. The Doppler frequency is the total estimated Doppler frequency offset without any geometric or mis-pointing corrections. The corresponding radial velocity is retrieved from the Doppler frequency after correcting the estimated total Doppler frequency for antenna mis-pointing as function of elevation angle and compensating for the attitude/orbit Doppler signal error. As for the OSW and OWI components, the RVL component also contains information on the block size used for estimation. The spatial coverage of the RVL product is equal to the spatial coverage of the corresponding Level-1 WV SLC or sub-images extracted from the Level-1 SM/IW/EW SLC products.

The figure below shows the flowchart of the RVL processing:
image-20231026-142430.png
Figure 13: Flowchart of RVL Algorithm
POD Sentinel-1 Products Specification

Generic introduction to POD products is available in overview of POD and in Sentinel-1 POD service.

Regarding access to Copernicus Sentinel-1 orbit files users are kindly invited to note that:

    The reference for accessing the Sentinel-1 orbit files is on the Copernicus Data Space Ecosystem

Precise orbits are determined based on the dual-frequency GPS (Global Positioning System) data delivered by dedicated geodetic-grade GPS receivers on-board the satellites. Several updates in the operational orbit determination were done during the years including an update of the GPS antenna reference point coordinates.
Products Description

The NRT orbital product (AUX_RESORB) generation is associated to the availability of a new GPS L0 file, which typically have a coverage of several orbits with significant overlaps. It has the coverage of the latest ascending node crossing (ANX) contained in the GPS L0 that triggered its generation plus 2 orbits backwards. Thus, the official POD NRT products for Sentinel-1 typically show overlaps of 1 or 2 orbits, depending on the coverage of the GPS L0 file.

The PRE orbital product (AUX_PREORB) generation is associated to the availability of a new GPS L0 file as the NRT case. It has the coverage of the latest ANX contained in the GPS L0 file that triggered its generation plus 4 orbits to the future. In this way, the official POD PRE products for Sentinel-1 typically show overlaps of 3 or 4 orbits, depending on the coverage of the GPS L0 file.

The STC orbital product (AUX_MOEORB) is generated with a timeliness of 31 hours and covers 26 hours, from 1 hour before the start of the previous day and 1 hour after the end of it. Therefore, there is an overlap of 2 hours between consecutive files.

The NTC orbital product (AUX_POEORB) is generated with a timeliness of 20 days and covers 26 hours, from 1 hour before the start of day N-20 until 1 hour after the end of day N-20 (being N the current day). Therefore, there is an overlap of 2 hours between consecutive NTC orbit files.

Each of these products has different timeliness and accuracy requirements, which are summarised in the following table:

Table 4: Sentinel-1 Copernicus POD Orbital Files Requirements
Enlarges the table by opening it in a full screen dialog
Open

Requirements of Orbital Products

Category
	

Latency
	

Orbit Accuracy

NRT Predicted (AUX_PREORB)
	

30 minutes
	

1 m (2D RMS 1-sigma)

NRT (AUX_RESORB)
	

45 minutes
	

10 cm (2D RMS 1-sigma)

STC (AUX_MOEORB)
	

31 hours
	

5 cm (3D RMS 1-sigma)

NTC (AUX_POEORB)
	

20 days
	

5 cm (3D RMS 1-sigma)

The timeliness and performance results of the precise orbital products are provided in the Metrics and Orbit Comparisons section.

Additionally, the GNSS L1B RINEX files for Sentinel-1 are delivered on a daily basis to the CGS with a latency of 7 days. These files contain the decoded GPS observations (pseudorange and carrier phase) following the International GNSS Service (IGS) RINEX ICD. The Sentinel-1 GNSS L1B RINEX data products are made available to the user community through the ESA CDSE.

Moreover, performing POD requires making use of the satellite attitude Quaternions files and validating the results obtained against an external reference solution. The Quaternions files and the NRT and NTC products generated by the CPOD Service are available to the user community through the ESA CDSE. It must be remarked that, at the beginning of year 2021, a Full Mission Reprocessing (FMR) was carried out on Sentinel-1 AUX_POEORB products, FMR 2021, in order to take into account the correction on the GPS antenna reference point location operationally performed on 29 to 30 July 2020 (see this document for detailed information).

The following table summarises the availability of all Sentinel-1 CPOD Service products routinely delivered to the CDSE:

Table 5: Sentinel-1 Copernicus POD Products in ESA CDSE
Enlarges the table by opening it in a full screen dialog
Open

S-1 CPOD Products

Product
	

Delivery Frequency (prod. per day)
	

Latency
	

Mission Availability

AUX_RESORB
	

~14
	

30 min*
	

Last 3 months

AUX_PREORB
	

~14
	

30 min*
	

Last 3 months

AUX_MOEORB
	

1
	

2 days
	

Last 3 months

AUX_POEORB
	

1
	

21 days
	

All mission

GNSS L1B RINEX Files
	

1
	

7 days
	

All mission

Quaternions Files
	

1
	

2 days
	

All mission

(*) w.r.t L0 availability


Satellite Parameters for POD

The Sentinel-1 mission is supported by two spacecraft, flying in the same orbital plane with a phase shift of 180º. An overall description of the satellite is available, showing the main instruments and the geometry of the satellites. Additionally, they are equipped with two dual frequency RUAG GPS receivers to allow POD in order to meet the stringent accuracy requirements of the mission.
https://sentinels.copernicus.eu/documents/247904/3376705/Sentinel-1-satellite-parameters.jpg
Figure 14: Drawing of Sentinel-1A showing the GPS receivers [Credits: ESA]

For Precise Orbit Determination processing it is of paramount importance to know in great detail the mass history of the satellite, the evolution of its centre of gravity, the manoeuvre history and the attitude information. Moreover, for GNSS based processing, the location and orientation of the antennae are required. In the case of the manoeuvre and mass history file (including the CoG position), available hereafter, a historical record is kept since the launch of the satellite:

    Sentinel-1A Mass History file and Sentinel-1C Mass History file

    Sentinel-1A Manoeuvre History file and Sentinel-1C Manoeuvre History file

For the convenience of users, another file combining information on manoeuvres and GNSS outages for the mission is provided below:

    Sentinel-1A Outage file and Sentinel-1C Outage file

Please note that all of the files provided above contain a header that explains the format of their content.

The GPS Antenna absolute phase centre offset and variations (PCO/PCV) of both GPS receivers is located in this ANTEX file.
Metrics

The official S1 Processing | S1 Processing POD Sentinel 1 Products Specification  in NRT are computed using 24 hours of data, whereas STC and NTC products are computed using 32 hours of data. This information and other modelling parameters are included in the Product Handbook. They are computed using P1/P2 code and L1/L2 phase ionosphere-free combinations. The number of used and rejected observations for each timeliness are shown hereafter:
image-20250506-155611.png
image-20250506-155735.png
image-20250506-155824.png
Figure 15-17: Number of used and rejected observations in Sentinel-1 NRT, STC and NTC processing

The RMS of residuals obtained from the processing of pseudorange and carrier phase after applying an ANTEX correction [https://files.igs.org/pub/station/general/antex14.txt] to the GPS antenna are shown below for all generated products:
image-20250506-160415.png
image-20250506-160440.png
image-20250506-160536.png
Figure 18-20: RMS of GNSS pseudorange residuals in Sentinel-1 NRT, STC and NTC processing
image-20250506-160654.png
image-20250506-160720.png
image-20250506-160737.png
Figures 21-23: RMS of GNSS carrier phase residuals in Sentinel-1 NRT, STC and NTC processing


Orbit Comparisons

The operational Sentinel-1 AUX_PREORB, AUX_RESORB and AUX_POEORB solutions from the CPOD Service are compared here against the combined solution (which is computed as a weighted mean of several external solutions provided by the CPOD QWG). The AUX_PREORB solution is divided into its two orbits in order to analyse the difference between the first and the second prediction.

In the following figures, the position accuracy of each orbit solution is shown (in 2D or 3D RMS depending on the requirement). Each figure is presented along with the distribution of the obtained accuracy metrics, where the percentiles of these metrics are calculated for different thresholds.

The period of time for AUX_PREORB and AUX_RESORB products correspond to the latest Yearly Regular Service Review. The period of the AUX_POEORB includes from the beginning of the mission. Orbit comparisons considered as outliers (i.e., those mostly generated from periods of time with manoeuvres or data gaps) have been filtered-out from the statistics shown below.

For AUX_PREORB and AUX_POEORB products some dispersion is quite visible due to the increased solar activity that we have suffered during the last years (see Analysis of Solar Activity Effects on POD 2024 document for further information).
AUX_PREORB (first orbit)

Sentinel-1 AUX_PREORB (1st orbit) products – Orbit comparisons against COMB solution [2D RMS; cm] (the accuracy requirement is shown with a blue line; vertical lines indicate periods of manoeuvres or data gaps)
image-20260316-090717.png
Figure 17: Sentinel-1 AUX_PREORB (1st orbit) products - Accuracy percentiles (they are calculated from the orbit comparisons against COMB solution [2D RMS])

 

Table 6:  Sentinel-1 AUX_PREORB (1st orbit) products – Accuracy percentiles (they are calculated from the orbit comparisons against COMB solution [2D RMS])
Enlarges the table by opening it in a full screen dialog
Open

Product Accuracy

 

Threshold
	

Percentage Fulfilment

Sentinel-1A
	

Sentinel-1C

5 cm
	

31.30 %
	

32.96 %

10 cm
	

76.64 %
	

76.64 %

20 cm
	

94.68 %
	

94.51 %

50 cm
	

99.06 %
	

98.69 %

100 cm
	

99.89 %
	

99.68 %

 
AUX_PREORB (second orbit)

Sentinel-1 AUX_PREORB (2nd orbit) products – Orbit comparisons against COMB solution [2D RMS; cm] (the accuracy requirement is shown with a blue line; vertical lines indicate periods of manoeuvres or data gaps) 
image-20260316-091829.png
Figure 18: Sentinel-1 AUX_PREORB (2nd orbit) products – Accuracy percentiles (they are calculated from the orbit comparisons against COMB solution [2D RMS])

Table 7: Sentinel-1 AUX_PREORB (2nd orbit) products – Accuracy percentiles (they are calculated from the orbit comparisons against COMB solution [2D RMS])
Enlarges the table by opening it in a full screen dialog
Open

Product Accuracy

 

Threshold
	

Percentage Fulfilment

Sentinel-1A
	

Sentinel-1C

5 cm
	

4.83 %
	

5.28 %

10 cm
	

26.25 %
	

28.40 %

20 cm
	

56.71 %
	

58.86 %

50 cm
	

88.42 %
	

88.81 %

100 cm
	

96.05 %
	

95.16 %
AUX_RESORB

Sentinel-1 AUX_RESORB products – Orbit comparisons against COMB solution [2D RMS; cm] (the accuracy requirement is shown with a blue line; vertical lines indicate periods of manoeuvres or data gaps)
image-20260316-091434.png
Figure 19: Sentinel-1 AUX_RESORB products - Accuracy percentiles (they are calculated from the orbit comparisons against COMB solution [2D RMS])

Table 8: Sentinel-1 AUX_RESORB products – Accuracy percentiles (they are calculated from the orbit comparisons against COMB solution [2D RMS])
Enlarges the table by opening it in a full screen dialog
Open

Product Accuracy

 

Threshold
	

Percentage Fulfilment

Sentinel-1A
	

 

1 cm
	

0.30 %
	

0.17 %

2 cm
	

31.98 %
	

26.91 %

3 cm
	

84.06 %
	

80.40 %

5 cm
	

98.96 %
	

99.07 %

10 cm
	

99.87 %
	

100.00 %
AUX_MOEORB

Sentinel-1 AUX_MOEORB products – Orbit comparisons against COMB solution [3D RMS; cm] (the accuracy requirement is shown with a blue line; vertical lines indicate periods of manoeuvres or data gaps)
image-20260316-092002.png
Figure 20: Sentinel-1 AUX_MOEORB products - Accuracy percentiles (they are calculated from the orbit comparisons against COMB solution [3D RMS])

Table 9: Sentinel-1 orbit comparisons – Accuracy percentiles (the accuracy percentiles are from AUX_MOEORB against COMB solution [3D RMS])
Enlarges the table by opening it in a full screen dialog
Open

Product Accuracy

 

Threshold
	

Percentage Fulfilment

Sentinel-1A
	

Sentinel-1C

2 cm
	

91.33 %
	

72.44 %

3 cm
	

99.46 %
	

99.29 %

5 cm
	

99.73 %
	

100.00 %

7 cm
	

99.73 %
	

100.00 %

10 cm
	

100.00 %
	

100.00 %
AUX_POEORB

Sentinel-1 AUX_POEORB products – Orbit comparisons against COMB solution [3D RMS; cm] (the accuracy requirement is shown with a blue line; vertical lines indicate periods of manoeuvres or data gaps)
image-20260316-092238.png
Figure 21: Sentinel-1 AUX_POEORB products - Accuracy percentiles (they are calculated from the orbit comparisons against COMB solution [3D RMS])

Table 10: Sentinel-1 orbit comparisons – Accuracy percentiles (the accuracy percentiles are from AUX_POEORB against COMB solution [3D RMS])
Enlarges the table by opening it in a full screen dialog
Open

Product Accuracy

 

Threshold
	

Percentage Fulfilment

Sentinel-1A
	

 

0.5 cm
	

52.46 %
	

50.00 %

1 cm
	

89.34 %
	

97.87 %

3 cm
	

100.00 %
	

100.00 %

5 cm
	

100.00 %
	

100.00 %

7 cm
	

100.00 %
	

100.00 %



    Copyright © 2026 European Space Agency Powered by Scroll Sites & Atlassian Confluence Manage cookie preferences 

Cookie Notice

We use cookies which are essential for you to access our website and to provide you with our services and allow us to measure and improve the performance of our website.
Accept all
Necessary only
Manage preferences
