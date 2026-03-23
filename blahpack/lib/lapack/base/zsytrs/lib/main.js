

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zsytrs = require( './zsytrs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zsytrs, 'ndarray', ndarray );


// EXPORTS //

module.exports = zsytrs;
