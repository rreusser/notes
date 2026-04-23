
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlar1v = require( './dlar1v.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlar1v, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlar1v;
