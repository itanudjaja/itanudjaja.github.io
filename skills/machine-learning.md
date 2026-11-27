---
layout: default
title: Machine Learning & MLOps
---

# Machine Learning & MLOps

## Demand forecasting under warehouse capacity constraints

How should a warehouse prioritise products when storage capacity is reduced, while minimising potential revenue loss?

Using Walmart point-of-sale data (2011–2016), developed daily demand forecasts and evaluated how different forecasting approaches could inform inventory allocation.

- Feature engineering: Developed lag, rolling-window, weekday, weekend and event-based features to capture intermittent demand patterns. Also accounted for product inactivity to avoid treating periods before a product became available as zero demand.

- Model comparison: Evaluated Prophet, LightGBM, Random Forest and Elastic Net using weighted absolute percentage error.

- Decision optimisation: Rather than relying on a single forecast, used the most pessimistic revenue-loss estimate across the four models to prioritise SKUs. Simulated progressive capacity reductions to identify where revenue loss began to increase significantly.


### Slides

<div class="viewer">
<input type="radio" name="d4" id="d4-1" checked>
<input type="radio" name="d4" id="d4-2">
<input type="radio" name="d4" id="d4-3">
<input type="radio" name="d4" id="d4-4">
<input type="radio" name="d4" id="d4-5">
<input type="radio" name="d4" id="d4-6">
<input type="radio" name="d4" id="d4-7">
<input type="radio" name="d4" id="d4-8">
<div class="stage">
<div class="frame"><img src="../slides/inventory/01.jpg" alt="Inventory forecast, slide 1 of 8"><label class="deck-arrow prev" for="d4-8" aria-label="Previous slide">&#8249;</label><label class="deck-arrow next" for="d4-2" aria-label="Next slide">&#8250;</label></div>
<div class="frame"><img src="../slides/inventory/02.jpg" alt="Inventory forecast, slide 2 of 8"><label class="deck-arrow prev" for="d4-1" aria-label="Previous slide">&#8249;</label><label class="deck-arrow next" for="d4-3" aria-label="Next slide">&#8250;</label></div>
<div class="frame"><img src="../slides/inventory/03.jpg" alt="Inventory forecast, slide 3 of 8"><label class="deck-arrow prev" for="d4-2" aria-label="Previous slide">&#8249;</label><label class="deck-arrow next" for="d4-4" aria-label="Next slide">&#8250;</label></div>
<div class="frame"><img src="../slides/inventory/04.jpg" alt="Inventory forecast, slide 4 of 8"><label class="deck-arrow prev" for="d4-3" aria-label="Previous slide">&#8249;</label><label class="deck-arrow next" for="d4-5" aria-label="Next slide">&#8250;</label></div>
<div class="frame"><img src="../slides/inventory/05.jpg" alt="Inventory forecast, slide 5 of 8"><label class="deck-arrow prev" for="d4-4" aria-label="Previous slide">&#8249;</label><label class="deck-arrow next" for="d4-6" aria-label="Next slide">&#8250;</label></div>
<div class="frame"><img src="../slides/inventory/06.jpg" alt="Inventory forecast, slide 6 of 8"><label class="deck-arrow prev" for="d4-5" aria-label="Previous slide">&#8249;</label><label class="deck-arrow next" for="d4-7" aria-label="Next slide">&#8250;</label></div>
<div class="frame"><img src="../slides/inventory/07.jpg" alt="Inventory forecast, slide 7 of 8"><label class="deck-arrow prev" for="d4-6" aria-label="Previous slide">&#8249;</label><label class="deck-arrow next" for="d4-8" aria-label="Next slide">&#8250;</label></div>
<div class="frame"><img src="../slides/inventory/08.jpg" alt="Inventory forecast, slide 8 of 8"><label class="deck-arrow prev" for="d4-7" aria-label="Previous slide">&#8249;</label><label class="deck-arrow next" for="d4-1" aria-label="Next slide">&#8250;</label></div>
</div>
<div class="thumbs">
<label for="d4-1"><img src="../slides/inventory/01.jpg" alt=""></label>
<label for="d4-2"><img src="../slides/inventory/02.jpg" alt=""></label>
<label for="d4-3"><img src="../slides/inventory/03.jpg" alt=""></label>
<label for="d4-4"><img src="../slides/inventory/04.jpg" alt=""></label>
<label for="d4-5"><img src="../slides/inventory/05.jpg" alt=""></label>
<label for="d4-6"><img src="../slides/inventory/06.jpg" alt=""></label>
<label for="d4-7"><img src="../slides/inventory/07.jpg" alt=""></label>
<label for="d4-8"><img src="../slides/inventory/08.jpg" alt=""></label>
</div>
</div>

## Late-delivery risk

How can logistics teams identify orders at risk of late delivery early enough to take preventive action?

Using approximately 100,000 Olist orders (2017–2018), developed a binary classification system to predict late deliveries at dispatch time, supporting operational decisions such as shipment expediting and customer notifications.

- Data engineering: Built a reproducible pipeline for data cleaning, transformation and feature preparation, maintaining separate target labels and model inputs.

- Model development: Trained and tuned Logistic Regression, Random Forest and XGBoost models, followed by probability calibration. Shortlisted models based on validation F1.

- Deployment strategy: Incorporated human approval into model promotion rather than automatically deploying the highest-scoring experimental model.

- Monitoring and retraining: Tracked F1 against the validation baseline, prediction drift and feature drift. Generated alerts for potential model deterioration, with retraining triggered through deliberate review rather than automatically.


### Slides

<div class="viewer">
<input type="radio" name="d5" id="d5-1" checked>
<input type="radio" name="d5" id="d5-2">
<input type="radio" name="d5" id="d5-3">
<input type="radio" name="d5" id="d5-4">
<input type="radio" name="d5" id="d5-5">
<input type="radio" name="d5" id="d5-6">
<input type="radio" name="d5" id="d5-7">
<input type="radio" name="d5" id="d5-8">
<input type="radio" name="d5" id="d5-9">
<div class="stage">
<div class="frame"><img src="../slides/late-delivery/01.jpg" alt="Late-delivery risk, slide 1 of 9"><label class="deck-arrow prev" for="d5-9" aria-label="Previous slide">&#8249;</label><label class="deck-arrow next" for="d5-2" aria-label="Next slide">&#8250;</label></div>
<div class="frame"><img src="../slides/late-delivery/02.jpg" alt="Late-delivery risk, slide 2 of 9"><label class="deck-arrow prev" for="d5-1" aria-label="Previous slide">&#8249;</label><label class="deck-arrow next" for="d5-3" aria-label="Next slide">&#8250;</label></div>
<div class="frame"><img src="../slides/late-delivery/03.jpg" alt="Late-delivery risk, slide 3 of 9"><label class="deck-arrow prev" for="d5-2" aria-label="Previous slide">&#8249;</label><label class="deck-arrow next" for="d5-4" aria-label="Next slide">&#8250;</label></div>
<div class="frame"><img src="../slides/late-delivery/04.jpg" alt="Late-delivery risk, slide 4 of 9"><label class="deck-arrow prev" for="d5-3" aria-label="Previous slide">&#8249;</label><label class="deck-arrow next" for="d5-5" aria-label="Next slide">&#8250;</label></div>
<div class="frame"><img src="../slides/late-delivery/05.jpg" alt="Late-delivery risk, slide 5 of 9"><label class="deck-arrow prev" for="d5-4" aria-label="Previous slide">&#8249;</label><label class="deck-arrow next" for="d5-6" aria-label="Next slide">&#8250;</label></div>
<div class="frame"><img src="../slides/late-delivery/06.jpg" alt="Late-delivery risk, slide 6 of 9"><label class="deck-arrow prev" for="d5-5" aria-label="Previous slide">&#8249;</label><label class="deck-arrow next" for="d5-7" aria-label="Next slide">&#8250;</label></div>
<div class="frame"><img src="../slides/late-delivery/07.jpg" alt="Late-delivery risk, slide 7 of 9"><label class="deck-arrow prev" for="d5-6" aria-label="Previous slide">&#8249;</label><label class="deck-arrow next" for="d5-8" aria-label="Next slide">&#8250;</label></div>
<div class="frame"><img src="../slides/late-delivery/08.jpg" alt="Late-delivery risk, slide 8 of 9"><label class="deck-arrow prev" for="d5-7" aria-label="Previous slide">&#8249;</label><label class="deck-arrow next" for="d5-9" aria-label="Next slide">&#8250;</label></div>
<div class="frame"><img src="../slides/late-delivery/09.jpg" alt="Late-delivery risk, slide 9 of 9"><label class="deck-arrow prev" for="d5-8" aria-label="Previous slide">&#8249;</label><label class="deck-arrow next" for="d5-1" aria-label="Next slide">&#8250;</label></div>
</div>
<div class="thumbs">
<label for="d5-1"><img src="../slides/late-delivery/01.jpg" alt=""></label>
<label for="d5-2"><img src="../slides/late-delivery/02.jpg" alt=""></label>
<label for="d5-3"><img src="../slides/late-delivery/03.jpg" alt=""></label>
<label for="d5-4"><img src="../slides/late-delivery/04.jpg" alt=""></label>
<label for="d5-5"><img src="../slides/late-delivery/05.jpg" alt=""></label>
<label for="d5-6"><img src="../slides/late-delivery/06.jpg" alt=""></label>
<label for="d5-7"><img src="../slides/late-delivery/07.jpg" alt=""></label>
<label for="d5-8"><img src="../slides/late-delivery/08.jpg" alt=""></label>
<label for="d5-9"><img src="../slides/late-delivery/09.jpg" alt=""></label>
</div>
</div>
