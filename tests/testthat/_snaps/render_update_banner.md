# render_update_banner produces expected HTML with NEWS link

    Code
      cat(as.character(render_update_banner(result, "ntrdWisconsin", action)))
    Output
      <div class="update-banner" role="status">
        <span class="update-banner-icon"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" class="bi bi-arrow-up-circle " style="height:1em;width:1em;fill:currentColor;vertical-align:-0.125em;" aria-hidden="true" role="img" ><path fill-rule="evenodd" d="M1 8a7 7 0 1 0 14 0A7 7 0 0 0 1 8zm15 0A8 8 0 1 1 0 8a8 8 0 0 1 16 0zm-7.5 3.5a.5.5 0 0 1-1 0V5.707L5.354 7.854a.5.5 0 1 1-.708-.708l3-3a.5.5 0 0 1 .708 0l3 3a.5.5 0 0 1-.708.708L8.5 5.707V11.5z"></path></svg></span>
        <span class="update-banner-text">Update available for ntrdWisconsin: 0.1.0 → 0.2.0.</span>
        <a href="https://example.com/NEWS.md" target="_blank" rel="noopener noreferrer" class="update-banner-news-link">What's new?</a>
        <button class="btn btn-default action-button btn-sm btn-primary update-banner-action" id="do_update" type="button"><span class="action-label">Update</span></button>
      </div>

