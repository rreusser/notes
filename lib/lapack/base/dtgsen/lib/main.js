
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dtgsen = require( './dtgsen.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtgsen, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtgsen;
