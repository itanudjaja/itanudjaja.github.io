---
layout: default
title: Computer vision
---

# Computer vision

Vision transformers, full fine-tuning, and parameter-efficient adaptation. 

## Fine-grained classification

We worked on a fine-grained classification task involving 10,000 classes, mostly plants and animals, with only 45 labelled training images per class. Many classes look almost identical, making it difficult for a pretrained model to distinguish between them.

A frozen Vision Transformer (ViT-B/16) provided a useful starting point, but its pretrained features were not enough. 56% of the training images were actually closer to another class's average feature representation than their own. There was also a wide gap in difficulty: the easiest 10% of classes achieved around 80% accuracy, while the hardest 10% scored only around 10%.

### Approach

With a training budget of approximately two GPU-hours, we explored two ways to adapt the pretrained model:
- Full fine-tuning (FFT): Updated all 93.6 million model parameters. We experimented with higher-resolution inputs and data augmentation to better capture fine-grained visual details. A two-resolution ensemble combined with a balanced assignment step achieved 79.84% validation accuracy.
- Parameter-efficient adaptation: Kept the backbone largely frozen, using Prefix Tuning, LoRA and AdaptFormer across different transformer blocks, with bias terms also trained. This required training only 1.66 million backbone parameters (1.93% of the backbone).

### Slides

<div class="viewer">
<input type="radio" name="d6" id="d6-1" checked>
<input type="radio" name="d6" id="d6-2">
<input type="radio" name="d6" id="d6-3">
<input type="radio" name="d6" id="d6-4">
<input type="radio" name="d6" id="d6-5">
<input type="radio" name="d6" id="d6-6">
<input type="radio" name="d6" id="d6-7">
<input type="radio" name="d6" id="d6-8">
<div class="stage">
<div class="frame"><img src="../slides/vision/01.jpg" alt="Fine-grained classification, slide 1 of 8"><label class="deck-arrow prev" for="d6-8" aria-label="Previous slide">&#8249;</label><label class="deck-arrow next" for="d6-2" aria-label="Next slide">&#8250;</label></div>
<div class="frame"><img src="../slides/vision/02.jpg" alt="Fine-grained classification, slide 2 of 8"><label class="deck-arrow prev" for="d6-1" aria-label="Previous slide">&#8249;</label><label class="deck-arrow next" for="d6-3" aria-label="Next slide">&#8250;</label></div>
<div class="frame"><img src="../slides/vision/03.jpg" alt="Fine-grained classification, slide 3 of 8"><label class="deck-arrow prev" for="d6-2" aria-label="Previous slide">&#8249;</label><label class="deck-arrow next" for="d6-4" aria-label="Next slide">&#8250;</label></div>
<div class="frame"><img src="../slides/vision/04.jpg" alt="Fine-grained classification, slide 4 of 8"><label class="deck-arrow prev" for="d6-3" aria-label="Previous slide">&#8249;</label><label class="deck-arrow next" for="d6-5" aria-label="Next slide">&#8250;</label></div>
<div class="frame"><img src="../slides/vision/05.jpg" alt="Fine-grained classification, slide 5 of 8"><label class="deck-arrow prev" for="d6-4" aria-label="Previous slide">&#8249;</label><label class="deck-arrow next" for="d6-6" aria-label="Next slide">&#8250;</label></div>
<div class="frame"><img src="../slides/vision/06.jpg" alt="Fine-grained classification, slide 6 of 8"><label class="deck-arrow prev" for="d6-5" aria-label="Previous slide">&#8249;</label><label class="deck-arrow next" for="d6-7" aria-label="Next slide">&#8250;</label></div>
<div class="frame"><img src="../slides/vision/07.jpg" alt="Fine-grained classification, slide 7 of 8"><label class="deck-arrow prev" for="d6-6" aria-label="Previous slide">&#8249;</label><label class="deck-arrow next" for="d6-8" aria-label="Next slide">&#8250;</label></div>
<div class="frame"><img src="../slides/vision/08.jpg" alt="Fine-grained classification, slide 8 of 8"><label class="deck-arrow prev" for="d6-7" aria-label="Previous slide">&#8249;</label><label class="deck-arrow next" for="d6-1" aria-label="Next slide">&#8250;</label></div>
</div>
<div class="thumbs">
<label for="d6-1"><img src="../slides/vision/01.jpg" alt=""></label>
<label for="d6-2"><img src="../slides/vision/02.jpg" alt=""></label>
<label for="d6-3"><img src="../slides/vision/03.jpg" alt=""></label>
<label for="d6-4"><img src="../slides/vision/04.jpg" alt=""></label>
<label for="d6-5"><img src="../slides/vision/05.jpg" alt=""></label>
<label for="d6-6"><img src="../slides/vision/06.jpg" alt=""></label>
<label for="d6-7"><img src="../slides/vision/07.jpg" alt=""></label>
<label for="d6-8"><img src="../slides/vision/08.jpg" alt=""></label>
</div>
</div>
