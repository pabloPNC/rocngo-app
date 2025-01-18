$(function() {
    function lower_input(el) {
        return $(el).find(".form-control").first();
    }
    function upper_input(el) {
        return $(el).find(".form-control").last();
    }
    let numRangeBinding = new Shiny.InputBinding();

    $.extend(numRangeBinding, {
        find : function(scope) {
            return $(scope).find('.numeric-range-input');
        },
        getValue : function(el) {
            let lower_input_val = lower_input(el).val();
            let upper_input_val = upper_input(el).val();
            return [parseInt(lower_input_val), parseInt(upper_input_val)];
        },
        setValue : function(el, value) {
            lower_input(el).val(value[0]);
            upper_input(el).val(value[1]);
        },
        receiveMessage : function(el, data) {
            if (data.hasOwnProperty('value')) {
                this.setValue(el, data.value);
            }
            if (data.hasOwnProperty('min')) {
                lower_input(el).attr('min', data.min)
                upper_input(el).attr('min', data.min)
            }
            if (data.hasOwnProperty('max')) {
                lower_input(el).attr('max', data.max)
                upper_input(el).attr('max', data.max)
            }
            if (data.hasOwnProperty('step')) {
                lower_input(el).attr('step', data.step)
                upper_input(el).attr('step', data.step)
            }
        },
        subscribe: function(el, callback) {
            $(el).on(
                "input.numRangeBinding",
                function(event) {
                    callback(true)
                }
            )
        }
    })
    Shiny.inputBindings.register(numRangeBinding, "ROCnGO.numRangeBinding")
});