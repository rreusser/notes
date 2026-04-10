
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dtgsna = require( './dtgsna.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtgsna, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtgsna;
