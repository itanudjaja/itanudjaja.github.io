---
layout: default
title: Natural language processing
---

# Natural language processing

Retrieval over real-world documents, with a focus on making generated answers relevant, grounded and traceable to their sources.

## Virtual Historian

Singapore's history is documented across articles and historical resources, but finding reliable answers to specific questions requires more than a general-purpose chatbot.

A general language model can miss local context or invent details when it lacks reliable information. Basic retrieval-augmented generation (RAG) helps by bringing in relevant documents, but simply passing retrieved passages to a language model does not guarantee that the final answer is accurate or supported by those sources.

Virtual Historian is a question-answering system that retrieves information from Singapore history articles and checks the supporting evidence before generating an answer.

### Approach

We built the system around Gemma 3 4B and experimented with different retrieval and verification techniques:

- Chunking: Compared different document chunk sizes, with around 500 tokens providing the best results.

- Hybrid retrieval: Combined dense vector search with keyword search. This delivered the largest improvement in retrieval recall by capturing both semantic similarity and specific historical terms.

- Reranking: Reordered retrieved passages to prioritise those most relevant to the question.

- Consistency checking: Added a verification step to check whether the generated answer was consistent with the retrieved evidence.

- Source citation: Required answers to reference their supporting sources, making it easier to trace where information came from.

### Slides

<div class="viewer">
<input type="radio" name="d3" id="d3-1" checked>
<input type="radio" name="d3" id="d3-2">
<input type="radio" name="d3" id="d3-3">
<input type="radio" name="d3" id="d3-4">
<input type="radio" name="d3" id="d3-5">
<input type="radio" name="d3" id="d3-6">
<input type="radio" name="d3" id="d3-7">
<input type="radio" name="d3" id="d3-8">
<input type="radio" name="d3" id="d3-9">
<div class="stage">
<div class="frame"><img src="../slides/historian/01.jpg" alt="Virtual Historian, slide 1 of 9"><label class="deck-arrow prev" for="d3-9" aria-label="Previous slide">&#8249;</label><label class="deck-arrow next" for="d3-2" aria-label="Next slide">&#8250;</label></div>
<div class="frame"><img src="../slides/historian/02.jpg" alt="Virtual Historian, slide 2 of 9"><label class="deck-arrow prev" for="d3-1" aria-label="Previous slide">&#8249;</label><label class="deck-arrow next" for="d3-3" aria-label="Next slide">&#8250;</label></div>
<div class="frame"><img src="../slides/historian/03.jpg" alt="Virtual Historian, slide 3 of 9"><label class="deck-arrow prev" for="d3-2" aria-label="Previous slide">&#8249;</label><label class="deck-arrow next" for="d3-4" aria-label="Next slide">&#8250;</label></div>
<div class="frame"><img src="../slides/historian/04.jpg" alt="Virtual Historian, slide 4 of 9"><label class="deck-arrow prev" for="d3-3" aria-label="Previous slide">&#8249;</label><label class="deck-arrow next" for="d3-5" aria-label="Next slide">&#8250;</label></div>
<div class="frame"><img src="../slides/historian/05.jpg" alt="Virtual Historian, slide 5 of 9"><label class="deck-arrow prev" for="d3-4" aria-label="Previous slide">&#8249;</label><label class="deck-arrow next" for="d3-6" aria-label="Next slide">&#8250;</label></div>
<div class="frame"><img src="../slides/historian/06.jpg" alt="Virtual Historian, slide 6 of 9"><label class="deck-arrow prev" for="d3-5" aria-label="Previous slide">&#8249;</label><label class="deck-arrow next" for="d3-7" aria-label="Next slide">&#8250;</label></div>
<div class="frame"><img src="../slides/historian/07.jpg" alt="Virtual Historian, slide 7 of 9"><label class="deck-arrow prev" for="d3-6" aria-label="Previous slide">&#8249;</label><label class="deck-arrow next" for="d3-8" aria-label="Next slide">&#8250;</label></div>
<div class="frame"><img src="../slides/historian/08.jpg" alt="Virtual Historian, slide 8 of 9"><label class="deck-arrow prev" for="d3-7" aria-label="Previous slide">&#8249;</label><label class="deck-arrow next" for="d3-9" aria-label="Next slide">&#8250;</label></div>
<div class="frame"><img src="../slides/historian/09.jpg" alt="Virtual Historian, slide 9 of 9"><label class="deck-arrow prev" for="d3-8" aria-label="Previous slide">&#8249;</label><label class="deck-arrow next" for="d3-1" aria-label="Next slide">&#8250;</label></div>
</div>
<div class="thumbs">
<label for="d3-1"><img src="../slides/historian/01.jpg" alt=""></label>
<label for="d3-2"><img src="../slides/historian/02.jpg" alt=""></label>
<label for="d3-3"><img src="../slides/historian/03.jpg" alt=""></label>
<label for="d3-4"><img src="../slides/historian/04.jpg" alt=""></label>
<label for="d3-5"><img src="../slides/historian/05.jpg" alt=""></label>
<label for="d3-6"><img src="../slides/historian/06.jpg" alt=""></label>
<label for="d3-7"><img src="../slides/historian/07.jpg" alt=""></label>
<label for="d3-8"><img src="../slides/historian/08.jpg" alt=""></label>
<label for="d3-9"><img src="../slides/historian/09.jpg" alt=""></label>
</div>
</div>
