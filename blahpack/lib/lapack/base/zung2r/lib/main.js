

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zung2r = require( './zung2r.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zung2r, 'ndarray', ndarray );


// EXPORTS //

module.exports = zung2r;
