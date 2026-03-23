

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zlahr2 = require( './zlahr2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlahr2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlahr2;
