
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dhsein = require( './dhsein.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dhsein, 'ndarray', ndarray );


// EXPORTS //

module.exports = dhsein;
