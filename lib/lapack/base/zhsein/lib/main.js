

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhsein = require( './zhsein.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhsein, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhsein;
