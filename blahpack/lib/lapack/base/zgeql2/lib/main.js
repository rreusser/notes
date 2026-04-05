

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgeql2 = require( './zgeql2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgeql2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgeql2;
