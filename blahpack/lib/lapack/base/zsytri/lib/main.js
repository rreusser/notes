
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zsytri = require( './zsytri.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zsytri, 'ndarray', ndarray );


// EXPORTS //

module.exports = zsytri;
