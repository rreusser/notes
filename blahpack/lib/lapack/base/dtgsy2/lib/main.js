'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dtgsy2 = require( './dtgsy2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtgsy2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtgsy2;
