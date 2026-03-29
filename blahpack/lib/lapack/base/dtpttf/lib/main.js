'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dtpttf = require( './dtpttf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtpttf, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtpttf;
