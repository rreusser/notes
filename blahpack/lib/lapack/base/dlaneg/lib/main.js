
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlaneg = require( './dlaneg.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlaneg, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlaneg;
