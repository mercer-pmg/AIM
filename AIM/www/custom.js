$(document).on("click", ".bootstrap-select .badge", function(event) {
    event.stopPropagation(); // Prevent dropdown from opening
    event.preventDefault();  // Block default picker behavior

    var value = $(this).text().trim(); // Get clicked badge's text
    var select = $("#strategy_category");

    // Get currently selected values
    var selectedValues = select.val();
    
    // Remove only the clicked value
    selectedValues = selectedValues.filter(v => v !== value);

    // Update pickerInput selections without reopening dropdown
    select.val(selectedValues).change();

    // Close the dropdown manually (this prevents Bootstrap from overriding)
    $(".bootstrap-select").removeClass("open");
});

