

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlaqsp = require( './zlaqsp.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlaqsp, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlaqsp;
