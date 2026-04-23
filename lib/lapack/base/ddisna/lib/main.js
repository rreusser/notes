

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ddisna = require( './ddisna.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ddisna, 'ndarray', ndarray );


// EXPORTS //

module.exports = ddisna;
