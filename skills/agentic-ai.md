---
layout: default
title: Agentic AI
---

# Agentic AI

## Finsight

Young investors increasingly turn to financial influencers for investment information. But how do you check whether an influencer's claim about a company is actually supported by the facts?

A single language model can produce a convincing answer without properly grounding it in evidence. Even retrieval-augmented generation (RAG), which brings in company filings and related documents, does not guarantee that the final verdict is supported by the retrieved evidence.

### Approach

FinSight uses a multi-agent workflow to break claim verification into separate tasks:

- Claim extraction: Identifies the company, financial metric or claim, and what needs to be verified.

- Evidence retrieval: Routes the claim to either a financial agent, which analyses company figures, or a narrative agent, which retrieves relevant passages from company filings and related documents.

- Verdict assessment: Evaluates whether the evidence supports, partially supports, or challenges the claim, or whether there is insufficient evidence to reach a conclusion.

- Auditing: Checks the verdict against the cited sources, including whether figures and statements in the assessment can actually be traced back to the evidence.

An important design choice was allowing the system to return Insufficient Evidence. When the available sources do not support a clear conclusion, it should say so rather than force a verdict.

### Slides

<div class="deck">
<img src="../slides/finsight/01.jpg" alt="Finsight, slide 1 of 7">
<img src="../slides/finsight/02.jpg" alt="Finsight, slide 2 of 7">
<img src="../slides/finsight/03.jpg" alt="Finsight, slide 3 of 7">
<img src="../slides/finsight/04.jpg" alt="Finsight, slide 4 of 7">
<img src="../slides/finsight/05.jpg" alt="Finsight, slide 5 of 7">
<img src="../slides/finsight/06.jpg" alt="Finsight, slide 6 of 7">
<img src="../slides/finsight/07.jpg" alt="Finsight, slide 7 of 7">
</div>


## 2Digit

2Digit's products provide investors with information, but there is an opportunity to give them a reason to return. We explored this through two products: AI SEC, delivered through Samsung mPOP, and NineMemos, 2Digit's own investment research platform.

The main question was how to deliver value earlier in the investor journey, rather than making users work to find it or asking them to sign in before they have seen what the platform offers.

### What we experimented with

We looked at the two products separately, as the information available to us was quite different.

- AI SEC: We analysed its 2025 usage data and found that most sessions started from stock or report pages, while repeat visits within a week were low. We explored an event-driven experience that brings together the companies affected, a short explanation of what happened, supporting evidence and relevant follow-up information.

- NineMemos: Without an existing usage baseline, we used surveys, interviews, competitor research and two rounds of usability testing to understand what investors needed. Our prototype brings relevant market information to the user before login, with a follow-up view highlighting what has changed since their last visit.

- Agent workflow: We also compared a free-form AI prompt with a more structured workflow. The latter breaks the task into steps: classifying events, summarising information, identifying affected companies and attaching supporting evidence. LangGraph manages the workflow, while predefined rules determine whether the content should be published, held for review or suppressed.

The work resulted in product concepts and prototypes for both platforms. 

### Slides

<div class="deck">
<img src="../slides/2digit/01.jpg" alt="2Digit, slide 1 of 17">
<img src="../slides/2digit/02.jpg" alt="2Digit, slide 2 of 17">
<img src="../slides/2digit/03.jpg" alt="2Digit, slide 3 of 17">
<img src="../slides/2digit/04.jpg" alt="2Digit, slide 4 of 17">
<img src="../slides/2digit/05.jpg" alt="2Digit, slide 5 of 17">
<img src="../slides/2digit/06.jpg" alt="2Digit, slide 6 of 17">
<img src="../slides/2digit/07.jpg" alt="2Digit, slide 7 of 17">
<img src="../slides/2digit/08.jpg" alt="2Digit, slide 8 of 17">
<img src="../slides/2digit/09.jpg" alt="2Digit, slide 9 of 17">
<img src="../slides/2digit/10.jpg" alt="2Digit, slide 10 of 17">
<img src="../slides/2digit/11.jpg" alt="2Digit, slide 11 of 17">
<img src="../slides/2digit/12.jpg" alt="2Digit, slide 12 of 17">
<img src="../slides/2digit/13.jpg" alt="2Digit, slide 13 of 17">
<img src="../slides/2digit/14.jpg" alt="2Digit, slide 14 of 17">
<img src="../slides/2digit/15.jpg" alt="2Digit, slide 15 of 17">
<img src="../slides/2digit/16.jpg" alt="2Digit, slide 16 of 17">
<img src="../slides/2digit/17.jpg" alt="2Digit, slide 17 of 17">
</div>
