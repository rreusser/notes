
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zpstrf = require( './zpstrf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zpstrf, 'ndarray', ndarray );


// EXPORTS //

module.exports = zpstrf;
