

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zlaqps = require( './zlaqps.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlaqps, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlaqps;
