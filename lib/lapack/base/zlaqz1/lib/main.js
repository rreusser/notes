
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlaqz1 = require( './zlaqz1.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlaqz1, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlaqz1;
